use std::{
    collections::HashMap,
    convert::{TryFrom, TryInto},
    error::Error,
    fmt::{self, Display, Formatter},
    ptr::null_mut,
};

use crate::{
    chunk::{Chunk, Opcode},
    class::ObjClass,
    closure::ObjClosure,
    compiler::compile,
    function::ObjFunction,
    instance::{ObjBoundMethod, ObjInstance},
    native::{native_clock, NativeFn, ObjNative},
    object::{Obj, ObjType, Object},
    string::ObjString,
    table::Table,
    upvalue::{ObjUpvalue, State},
    value::{TypeError, Value},
};

macro_rules! binop {
    ($vm:expr, $op:expr) => {{
        let r = VM::pop($vm);
        let l = VM::pop($vm);
        let new = $op(&l, &r).into();
        VM::push($vm, new);
    }};
}

macro_rules! binop_err {
    ($vm:expr, $op:expr) => {{
        let r = VM::pop($vm);
        let l = VM::pop($vm);

        match $op(l, r) {
            Ok(v) => {
                VM::push($vm, v);
            }
            Err(TypeError::Numbers) => {
                VM::runtime_error($vm, "Operands must be numbers.");
                return Err(InterpretError::Runtime);
            }
            Err(TypeError::AddMismatch) => {
                VM::runtime_error(
                    $vm,
                    "Operands must be two numbers or two strings.",
                );
                return Err(InterpretError::Runtime);
            }
        }
    }};
}

macro_rules! binop_heap {
    ($vm:expr, $op:expr) => {{
        let r = $vm.peek(0);
        let l = $vm.peek(1);
        let result = $op(l, r, $vm);

        match result {
            Ok(v) => {
                $vm.pop();
                $vm.pop();
                VM::push($vm, v);
            }
            Err(TypeError::Numbers) => {
                $vm.pop();
                $vm.pop();
                VM::runtime_error($vm, "Operands must be numbers.");
                return Err(InterpretError::Runtime);
            }
            Err(TypeError::AddMismatch) => {
                $vm.pop();
                $vm.pop();
                VM::runtime_error(
                    $vm,
                    "Operands must be two numbers or two strings.",
                );
                return Err(InterpretError::Runtime);
            }
        }
    }};
}

#[derive(Debug)]
pub enum InterpretError {
    Runtime,
    Compile,
}

impl Display for InterpretError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for InterpretError {}

/// # Safety
///
/// Note that `ip` points into `chunk`. A [`Chunk`]'s memory can move around, so
/// we need to be sure that we're updating `ip` after any time we mutate
/// `chunk`.
///
/// Don't reassign the heap.
pub struct VM {
    frames: [CallFrame; VM::FRAMES_MAX],
    frame_count: usize,
    stack: [Value; VM::STACK_MAX],
    stack_top: usize,
    globals: Table,
    open_upvalues: Vec<*mut ObjUpvalue>,

    // heap
    bytes_allocated: usize,
    init_string: *mut ObjString,
    next_gc: usize,
    pause_gc: bool,
    objects: *mut Obj,
    strings: HashMap<u64, *mut ObjString>,
    gray_stack: Vec<*mut Obj>,
}

impl Drop for VM {
    fn drop(&mut self) {
        self.objects = null_mut();
        self.init_string = null_mut();
        let mut cursor = self.objects;

        while !cursor.is_null() {
            cursor = unsafe {
                let next = (*cursor).next;
                Obj::free(cursor);
                next
            }
        }
    }
}

impl VM {
    const FRAMES_MAX: usize = 64;
    const STACK_MAX: usize = VM::FRAMES_MAX * (u8::MAX as usize + 1);

    pub fn new() -> VM {
        let mut vm = VM {
            frames: [CallFrame::UNINIT; VM::FRAMES_MAX],
            frame_count: 0,
            stack: [Value::Nil; VM::STACK_MAX],
            stack_top: 0,
            globals: Table::new(),
            open_upvalues: Vec::new(),
            bytes_allocated: 0,
            next_gc: 1024 * 1024,
            pause_gc: true, // gc paused until setup is complete.
            objects: null_mut(),
            strings: HashMap::new(),
            gray_stack: Vec::new(),
            init_string: null_mut(),
        };

        vm.define_native("clock", native_clock);
        vm.init_string = ObjString::copy("init", &mut vm);

        vm.pause_gc = false;
        vm
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretError> {
        self.pause_gc = true;
        let function = compile(source, self);
        self.pause_gc = false;

        if function.is_null() {
            return Err(InterpretError::Compile);
        }

        // We wrap this in the stack in case ObjClosure::new allocating makes
        // the GC collect.
        self.push(function.into());
        let closure = ObjClosure::new(function, self);
        self.pop();

        self.push(closure.into());
        self.call(closure, 0)?;

        self.run()
    }

    fn reset_stack(&mut self) {
        self.stack_top = 0;
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    pub fn peek(&self, distance: usize) -> Value {
        self.stack[self.stack_top - 1 - distance]
    }

    fn pop(&mut self) -> Value {
        if self.stack_top == 0 {
            Value::Nil
        } else {
            self.stack_top -= 1;
            self.stack[self.stack_top]
        }
    }

    pub fn run(&mut self) -> Result<(), InterpretError> {
        loop {
            if cfg!(feature = "debug_trace") {
                let ip = self.frame().ip;
                let op = Opcode::try_from(unsafe {
                    (*(self.closure().function)).chunk.code[ip]
                })
                .map(Opcode::to_str)
                .unwrap_or("<invalid>");
                println!(
                    "ip {:4} {:<16} bp: {}, stack: {:?}",
                    ip,
                    op,
                    self.frame().bp,
                    &self.stack[0..self.stack_top],
                );
            }

            let instruction = self
                .read_byte()
                .try_into()
                .map_err(|_| InterpretError::Runtime)?;

            match instruction {
                Opcode::Constant => {
                    let constant = self.read_constant();
                    self.push(constant);
                }

                Opcode::Return => {
                    let result = self.pop();

                    self.close_upvalues(self.frame().bp);

                    let previous_bp = self.frame().bp;
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        return Ok(());
                    }

                    self.stack_top = previous_bp;
                    self.push(result);
                }

                Opcode::Negate => {
                    if !self.peek(0).is_number() {
                        self.runtime_error("Operand must be a number.");
                        return Err(InterpretError::Runtime);
                    }

                    let value = Value::from(-self.pop().as_number());
                    self.push(value)
                }

                Opcode::Not => {
                    let b = self.pop();
                    self.push(b.is_falsey().into())
                }

                Opcode::DefineGlobal => {
                    let key = self.read_string();
                    self.globals.set(key, self.peek(0));
                    self.pop();
                }

                Opcode::GetGlobal => {
                    let name = self.read_string();
                    if let Some(value) = self.globals.get(name) {
                        self.push(value)
                    } else {
                        let name = unsafe { (&*name).as_str() };
                        self.runtime_error(format!(
                            "Undefined variable '{}'.",
                            name
                        ));
                        return Err(InterpretError::Runtime);
                    }
                }

                Opcode::SetGlobal => {
                    let name = self.read_string();

                    if self.globals.set(name, self.peek(0)) {
                        self.globals.delete(name);
                        self.runtime_error(format!(
                            "Undefined variable '{}'.",
                            unsafe { &*name }.as_str(),
                        ));
                        return Err(InterpretError::Runtime);
                    }
                }

                Opcode::GetLocal => {
                    let slot = self.read_byte() as usize;
                    let value = self.stack[self.frame().bp + slot];
                    self.push(value);
                }

                Opcode::SetLocal => {
                    let slot = self.read_byte() as usize;
                    let value = self.peek(0);
                    self.stack[self.frame().bp + slot] = value;
                }

                Opcode::GetUpvalue => {
                    let slot = self.read_byte() as usize;

                    unsafe {
                        let upvalue = self.closure().upvalues[slot]
                            .as_mut()
                            .expect("cannot get null upvalue");
                        let value = upvalue.get();
                        self.push(value);
                    }
                }

                Opcode::SetUpvalue => {
                    let slot = self.read_byte() as usize;
                    let value = self.peek(0);

                    unsafe {
                        self.closure().upvalues[slot]
                            .as_mut()
                            .expect("cannot set null upvalue")
                            .set(value)
                    };
                }

                Opcode::GetProperty => {
                    if !self.peek(0).is_a::<ObjInstance>() {
                        self.runtime_error("Only instances have properties.");
                        return Err(InterpretError::Runtime);
                    }

                    let mut instance_value = self.peek(0);
                    let instance =
                        instance_value.as_a_mut::<ObjInstance>().unwrap();
                    let name = self.read_string();

                    if let Some(value) = instance.fields.get(name) {
                        self.pop();
                        self.push(value);
                        continue;
                    }

                    self.bind_method(instance.class, name)?;
                }

                Opcode::SetProperty => {
                    if let Some(instance) =
                        self.peek(1).as_a_mut::<ObjInstance>()
                    {
                        let name = self.read_string();
                        let value = self.peek(0);
                        instance.fields.set(name, value);
                        self.pop();
                        self.pop();
                        self.push(value);
                    } else {
                        self.runtime_error("Only instances have fields.");
                        return Err(InterpretError::Runtime);
                    }
                }

                Opcode::GetSuper => {
                    let name = self.read_string();
                    let mut superclass_value = self.pop();
                    let superclass = superclass_value.as_a_mut().unwrap();

                    self.bind_method(superclass, name)?;
                }

                Opcode::JumpIfFalse => {
                    let offset = self.read_u16() as usize;
                    if self.peek(0).is_falsey() {
                        self.frame_mut().ip += offset;
                    }
                }

                Opcode::Jump => {
                    let offset = self.read_u16() as usize;
                    self.frame_mut().ip += offset;
                }

                Opcode::Loop => {
                    let offset = self.read_u16() as usize;
                    self.frame_mut().ip -= offset;
                }

                Opcode::Call => {
                    let arg_count = self.read_byte();
                    self.call_value(
                        self.peek(arg_count as usize),
                        arg_count as usize,
                    )?;
                }

                Opcode::Closure => {
                    let mut value = self.read_constant();
                    let function = value
                        .as_a_mut::<ObjFunction>()
                        .expect("cannot make closure from null function");

                    let closure = unsafe {
                        ObjClosure::new(function, self).as_mut().unwrap()
                    };

                    self.push(Value::from(closure as *mut ObjClosure));

                    for i in 0..closure.upvalue_count {
                        let is_local = self.read_byte() == true as _;
                        let index = self.read_byte() as usize;

                        if is_local {
                            let stack_index = self.frame().bp + index;
                            let local = &mut self.stack[stack_index];
                            let local = local as _;
                            closure.upvalues[i] = self.capture_upvalue(local);
                        } else {
                            closure.upvalues[i] =
                                self.closure().upvalues[index];
                        }
                    }
                }

                Opcode::CloseUpvalue => {
                    self.close_upvalues(self.stack_top - 1);
                    self.pop();
                }

                Opcode::Class => {
                    let name = self.read_string();
                    let class = ObjClass::new(name, self);
                    self.push(class.into())
                }

                Opcode::Method => {
                    let name = self.read_string();
                    self.define_method(name);
                }

                Opcode::Invoke => {
                    let method = self.read_string();
                    let arg_count = self.read_byte() as usize;

                    self.invoke(method, arg_count)?;
                }

                Opcode::SuperInvoke => {
                    let method = self.read_string();
                    let arg_count = self.read_byte() as usize;

                    let mut superclass_value = self.pop();
                    let superclass =
                        superclass_value.as_a_mut::<ObjClass>().unwrap();

                    self.invoke_from_class(superclass, method, arg_count)?;
                }

                Opcode::Inherit => {
                    let superclass_value = self.peek(1);

                    if let Some(superclass) =
                        superclass_value.as_a::<ObjClass>()
                    {
                        let mut subclass_value = self.peek(0);
                        let subclass =
                            subclass_value.as_a_mut::<ObjClass>().unwrap();

                        subclass.methods.add_all(&superclass.methods);

                        self.pop();
                    } else {
                        self.runtime_error("Superclass must be a class.");
                        return Err(InterpretError::Runtime);
                    }
                }

                Opcode::Nil => self.push(Value::Nil),
                Opcode::True => self.push(Value::from(true)),
                Opcode::False => self.push(Value::from(false)),
                Opcode::Add => binop_heap!(self, Value::add),
                Opcode::Sub => binop_err!(self, std::ops::Sub::sub),
                Opcode::Mul => binop_err!(self, std::ops::Mul::mul),
                Opcode::Div => binop_err!(self, std::ops::Div::div),
                Opcode::Equal => binop!(self, std::cmp::PartialEq::eq),
                Opcode::Greater => binop_err!(self, Value::greater_than),
                Opcode::Less => binop_err!(self, Value::less_than),
                Opcode::Print => println!("{}", self.pop()),
                Opcode::Pop => {
                    self.pop();
                }
            }
        }
    }

    fn call_value(
        &mut self,
        mut callee: Value,
        arg_count: usize,
    ) -> Result<(), InterpretError> {
        if let Some(callee) = callee.as_a_mut::<ObjClosure>() {
            self.call(callee, arg_count)
        } else if let Some(native) = callee.as_a_mut::<ObjNative>() {
            let result = (native.function)(self);

            self.stack_top -= arg_count + 1;
            self.push(result);
            Ok(())
        } else if let Some(class) = callee.as_a_mut::<ObjClass>() {
            self.stack[self.stack_top - arg_count - 1] =
                Value::from(ObjInstance::new(class, self));

            if let Some(mut initializer_value) =
                class.methods.get(self.init_string)
            {
                let initializer =
                    initializer_value.as_a_mut::<ObjClosure>().unwrap();
                self.call(initializer, arg_count)
            } else if arg_count != 0 {
                self.runtime_error(format!(
                    "Expected 0 arguments but got {}.",
                    arg_count
                ));
                Err(InterpretError::Runtime)
            } else {
                Ok(())
            }
        } else if let Some(bound) = callee.as_a_mut::<ObjBoundMethod>() {
            self.stack[self.stack_top - arg_count - 1] = bound.receiver;
            self.call(bound.method, arg_count)
        } else {
            self.runtime_error("Can only call functions and classes.");
            Err(InterpretError::Runtime)
        }
    }

    fn call(
        &mut self,
        closure: *mut ObjClosure,
        arg_count: usize,
    ) -> Result<(), InterpretError> {
        let closure = unsafe { &mut *closure };
        let function = closure.function();
        let arity = function.arity;

        if arg_count != arity {
            self.runtime_error(format!(
                "Expected {} arguments but got {}.",
                arity, arg_count,
            ));
            return Err(InterpretError::Runtime);
        }

        if self.frame_count == VM::FRAMES_MAX {
            self.runtime_error("Stack overflow.");
            return Err(InterpretError::Runtime);
        }

        let frame = CallFrame {
            closure,
            ip: 0,
            bp: self.stack_top - arg_count - 1,
        };

        self.frames[self.frame_count] = frame;
        self.frame_count += 1;

        Ok(())
    }

    fn invoke(
        &mut self,
        name: *mut ObjString,
        arg_count: usize,
    ) -> Result<(), InterpretError> {
        let mut receiver = self.peek(arg_count);

        if let Some(instance) = receiver.as_a_mut::<ObjInstance>() {
            if let Some(value) = instance.fields.get(name) {
                self.stack[self.stack_top - arg_count - 1] = value;
                return self.call_value(value, arg_count);
            }

            let class = unsafe { instance.class.as_mut().unwrap() };
            self.invoke_from_class(class, name, arg_count)
        } else {
            self.runtime_error("Only instances have methods.");
            Err(InterpretError::Runtime)
        }
    }

    fn invoke_from_class(
        &mut self,
        class: &mut ObjClass,
        name: *mut ObjString,
        arg_count: usize,
    ) -> Result<(), InterpretError> {
        if let Some(mut method_value) = class.methods.get(name) {
            let method = method_value.as_a_mut::<ObjClosure>().unwrap();
            self.call(method, arg_count)
        } else {
            let name = unsafe { name.as_ref().unwrap().as_str() };
            self.runtime_error(format!("Undefined property '{}'.", name));
            Err(InterpretError::Runtime)
        }
    }

    fn capture_upvalue(&mut self, local: *mut Value) -> *mut ObjUpvalue {
        // had to do something a little different here since we aren't using a
        // linked list.

        fn key(u: &*mut ObjUpvalue) -> *mut Value {
            unsafe {
                u.as_mut()
                    .expect("null upvalue in open upvalue list")
                    .location()
            }
        }

        match self
            .open_upvalues
            .as_slice()
            .binary_search_by_key(&local, key)
        {
            Ok(index) => self.open_upvalues[index],
            Err(index) => {
                let new_upvalue = ObjUpvalue::new(local, self);
                self.open_upvalues.insert(index, new_upvalue);
                new_upvalue
            }
        }
    }

    /// Traverse `open_upvalues` and pop and close upvalues.
    ///
    /// We want to close upvalues which are at or past `last`.
    fn close_upvalues(&mut self, last: usize) {
        let last: *mut Value = &mut self.stack[last] as _;

        while let Some(upvalue) = self.open_upvalues.pop() {
            let u = unsafe { upvalue.as_mut() }
                .expect("null upvalue in open_upvalues");

            if u.location() >= last {
                u.close();
            } else {
                self.open_upvalues.push(upvalue);
                break;
            }
        }
    }

    fn bind_method(
        &mut self,
        class: *mut ObjClass,
        name: *mut ObjString,
    ) -> Result<(), InterpretError> {
        let class = unsafe { class.as_mut().unwrap() };
        if let Some(mut method_value) = class.methods.get(name) {
            let method = method_value.as_a_mut().unwrap();
            let bound = ObjBoundMethod::new(self.peek(0), method, self);
            self.pop();
            self.push(Value::from(bound));
            Ok(())
        } else {
            let name = unsafe { name.as_ref().unwrap() }.as_str();
            self.runtime_error(format!("Undefined property '{}'.", name));
            Err(InterpretError::Runtime)
        }
    }

    fn define_method(&mut self, name: *mut ObjString) {
        let method = self.peek(0);
        let mut class_value = self.peek(1);
        let class = class_value.as_a_mut::<ObjClass>().unwrap();
        class.methods.set(name, method);
        self.pop();
    }

    fn frame(&self) -> &CallFrame {
        &self.frames[self.frame_count - 1]
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        &mut self.frames[self.frame_count - 1]
    }

    fn closure(&self) -> &ObjClosure {
        unsafe { self.frame().closure.as_ref().unwrap() }
    }

    fn function(&self) -> &ObjFunction {
        unsafe { self.closure().function.as_ref().expect("function is null") }
    }

    fn chunk(&self) -> &Chunk {
        &self.function().chunk
    }

    fn read_byte(&mut self) -> u8 {
        let ip = self.frame().ip;
        let byte = self.chunk().code[ip];
        self.frame_mut().ip += 1;
        byte
    }

    fn read_u16(&mut self) -> u16 {
        let first = self.read_byte();
        let second = self.read_byte();
        u16::from_ne_bytes([first, second])
    }

    fn read_constant(&mut self) -> Value {
        let byte = self.read_byte() as usize;
        self.function().chunk.constants[byte]
    }

    fn read_string(&mut self) -> *mut ObjString {
        let mut val = self.read_constant();
        let obj = val.as_obj_mut();
        obj.as_a_mut().unwrap()
    }

    fn define_native(&mut self, name: &str, function: NativeFn) {
        let name = ObjString::copy(name, self);
        self.push(name.into());

        let native = ObjNative::new(function, self).into();
        self.push(native);

        self.globals.set(name, native);

        self.pop();
        self.pop();
    }

    fn runtime_error(&mut self, msg: impl AsRef<str>) {
        eprintln!("{}", msg.as_ref());

        for i in (0..(self.frame_count)).rev() {
            let frame = self.frames[i];
            let f = frame.closure().function();
            let line = f.chunk.lines[frame.ip - 1];
            let name = f.name().unwrap_or("script");
            eprintln!("[line {}] in {}", line, name);
        }

        if cfg!(feature = "debug_trace") {
            let f = self.function();
            f.chunk
                .disassemble(f.name().unwrap_or("<unnamed functions>"));
        }

        self.reset_stack();
    }
}

// Heap methods
impl VM {
    const GC_HEAP_GROWTH_FACTOR: usize = 2;

    pub unsafe fn allocate<T: Object>(
        &mut self,
        make_obj: impl FnOnce(Obj) -> T,
    ) -> *mut T {
        self.bytes_allocated += std::mem::size_of::<T>();

        if !cfg!(feature = "debug_stress_gc") {
            self.collect_garbage();
        }

        if self.bytes_allocated > self.next_gc {
            self.collect_garbage();
        }

        let obj = Obj {
            obj_type: T::TAG,
            is_marked: false,
            next: self.objects,
        };

        let ptr = Box::into_raw(Box::new(make_obj(obj)));

        self.objects = ptr as *mut Obj;

        if cfg!(feature = "debug_log_gc") {
            println!(
                "0x{:X} allocate {} for {:?}\n",
                ptr as usize,
                std::mem::size_of::<T>(),
                T::TAG
            );
        }
        ptr
    }

    pub fn intern(&mut self, hash: u64, obj: *mut ObjString) {
        self.strings.insert(hash, obj);
    }

    pub fn get_interned(&self, hash: u64) -> Option<*mut ObjString> {
        self.strings.get(&hash).cloned()
    }

    fn collect_garbage(&mut self) {
        if self.pause_gc {
            return;
        }

        if cfg!(feature = "debug_log_gc") {
            println!("-- gc begin\n");
        }
        let before = self.bytes_allocated;

        self.mark_roots();
        self.trace_reference();
        self.globals.remove_white_strings();
        unsafe { self.sweep() };

        self.next_gc = self.bytes_allocated * VM::GC_HEAP_GROWTH_FACTOR;

        if cfg!(feature = "debug_log_gc") {
            println!("-- gc end\n");
            println!(
                "   collected {} bytes (from {} to {}) next at {}",
                self.bytes_allocated - before,
                before,
                self.bytes_allocated,
                self.next_gc
            );
        }
    }

    fn mark_roots(&mut self) {
        for slot in 0..self.stack_top {
            let value = self.stack[slot];
            self.mark_value(value);
        }

        for slot in 0..self.open_upvalues.len() {
            let upvalue = self.open_upvalues[slot];
            if let Some(u) = unsafe { upvalue.as_mut() } {
                self.mark_object(Obj::upcast_mut(u));
            }
        }

        // GC is paused during compilation while `compile()` is called, so it
        // will ever trigger GC. Since it doesn't retain any GC references
        // between calls (where would it) it's safe to not traverse for GC
        // roots, so long as compilation doesn't produce so many objects we run
        // out of memory.
        //
        // self.mark_compiler_roots();

        let init_string = unsafe { self.init_string.as_mut().unwrap() };
        self.mark_object(Obj::upcast_mut(init_string));
        self.mark_global_table();
    }
}

impl VM {
    fn mark_value(&mut self, mut value: Value) {
        if value.is_obj() {
            let obj = value.as_obj_mut();
            self.mark_object(obj);
        }
    }

    fn mark_object(&mut self, obj: &mut Obj) {
        if obj.is_marked {
            return;
        }

        if cfg!(feature = "debug_log_gc") {
            println!("0x{:X} mark {:?}", obj as *mut _ as usize, obj);
        }

        obj.is_marked = true;
        self.gray_stack.push(obj);
    }

    fn mark_table(&mut self, table: &mut Table) {
        for (key, value) in table.entries.values() {
            let k = unsafe {
                let k = *key;
                (k as *mut ObjString)
                    .as_mut()
                    .expect("non-null keys in global table")
            };
            self.mark_object(Obj::upcast_mut(k));
            self.mark_value(*value);
        }
    }

    fn mark_slice(&mut self, slice: &mut [Value]) {
        for value in slice {
            self.mark_value(*value);
        }
    }

    fn mark_global_table(&mut self) {
        let mut temp = Table::new();
        std::mem::swap(&mut temp, &mut self.globals);
        self.mark_table(&mut temp);
        std::mem::swap(&mut temp, &mut self.globals);
    }

    pub(crate) fn trace_reference(&mut self) {
        while let Some(object) = self.gray_stack.pop() {
            self.blacken_object(object);
        }
    }

    fn blacken_object(&mut self, object_ptr: *mut Obj) {
        let object = unsafe { object_ptr.as_mut() }.unwrap();

        if cfg!(feature = "debug_log_gc") {
            println!("0x{:X} blacken {:?}", object_ptr as usize, object);
        }

        match object.obj_type() {
            // Types with no references.
            ObjType::String | ObjType::Native => {}

            ObjType::Upvalue => {
                if let State::Closed(v) =
                    object.as_a_mut::<ObjUpvalue>().unwrap().state
                {
                    self.mark_value(v);
                }
            }
            ObjType::Function => {
                let function = object.as_a_mut::<ObjFunction>().unwrap();
                unsafe {
                    function
                        .name
                        .as_mut()
                        .map(|n| self.mark_object(Obj::upcast_mut(n)))
                };

                self.mark_slice(&mut function.chunk.constants);
            }
            ObjType::Closure => {
                let closure = object.as_a_mut::<ObjClosure>().unwrap();
                self.mark_object(Obj::upcast_mut(closure.function_mut()));
                for upvalue in closure.upvalues[0..closure.upvalue_count]
                    .iter()
                    .filter_map(|u| unsafe { u.as_mut() })
                {
                    self.mark_object(Obj::upcast_mut(upvalue));
                }
            }
            ObjType::Class => {
                let class = object.as_a_mut::<ObjClass>().unwrap();
                let name = unsafe { class.name.as_mut().unwrap() };
                self.mark_object(Obj::upcast_mut(name));
                self.mark_table(&mut class.methods);
            }
            ObjType::Instance => unsafe {
                let instance = object.as_a_mut::<ObjInstance>().unwrap();
                let class = instance.class.as_mut().unwrap();
                self.mark_object(Obj::upcast_mut(class));
                self.mark_table(&mut instance.fields);
            },
            ObjType::BoundMethod => unsafe {
                let bound = object.as_a_mut::<ObjBoundMethod>().unwrap();
                self.mark_value(bound.receiver);
                let method = bound.method.as_mut().unwrap();
                self.mark_object(Obj::upcast_mut(method));
            },
        }
    }

    unsafe fn sweep(&mut self) {
        let mut previous = null_mut();
        let mut object = self.objects;

        while !object.is_null() {
            let obj = object.as_mut().unwrap();
            if obj.is_marked {
                obj.is_marked = false;
                previous = object;
                object = obj.next;
            } else {
                let unreached = object;
                object = obj.next;

                if let Some(prev) = previous.as_mut() {
                    prev.next = object;
                } else {
                    self.objects = object
                }

                Obj::free(unreached);
            }
        }
    }
}

// Lots of changes to this to use indexes instead of pointers.
#[derive(Clone, Copy)]
struct CallFrame {
    /// GC pointer to function being called.
    closure: *mut ObjClosure,

    /// Offset from function's code instead of a pointer.
    ip: usize,

    /// The stack index where the current frame's slots start.
    /// self.stack[frame.pb] should point to the frame's closure.
    bp: usize,
}

impl CallFrame {
    const UNINIT: CallFrame = CallFrame {
        closure: null_mut(),
        ip: 0,
        bp: 0,
    };

    fn closure(&self) -> &ObjClosure {
        unsafe { &*self.closure }
    }
}

impl fmt::Debug for CallFrame {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("CallFrame")
            .field("closure", unsafe {
                &self.closure.as_ref().unwrap().function.as_ref().unwrap()
            })
            .field("ip", &self.ip)
            .field("bp", &self.bp)
            .finish()
    }
}
