use std::ptr::null_mut;

use crate::{
    chunk::{Chunk, Opcode},
    closure::ObjClosure,
    function::{FunctionKind, ObjFunction},
    parser::{Parser, Precedence, Rule},
    scanner::{Kind, Scanner, Token},
    string::ObjString,
    value::Value,
    vm::VM,
};

pub fn compile(source: &str, heap: &mut VM) -> *mut ObjFunction {
    let mut parser = Parser::new(Scanner::new(source));
    parser.advance(); // prime the pump

    let mut context = Context::new(&mut parser, heap);

    let function = context.compile_top_level();

    if context.parser.had_error {
        null_mut()
    } else {
        function
    }
}

pub struct Context<'input, 'heap> {
    parser: &'input mut Parser<'input>,
    heap: &'heap mut VM,

    compilers: Vec<Compiler<'input>>,
}

impl<'input, 'heap> Context<'input, 'heap> {
    fn new(parser: &'input mut Parser<'input>, heap: &'heap mut VM) -> Self {
        Context {
            parser,
            heap,
            compilers: Vec::new(),
        }
    }

    fn compile_top_level(&mut self) -> *mut ObjFunction {
        let top_level = Compiler::new_top_level(&mut self.heap);
        self.compilers.push(top_level);

        while !self.matches(Kind::Eof) {
            self.declaration();
        }

        self.parser.consume(Kind::Eof, "Expect end of expression");

        self.end_compiler().function
    }

    fn current(&self) -> &Compiler<'input> {
        self.compilers.last().expect("compiler stack empty")
    }

    fn current_mut(&mut self) -> &mut Compiler<'input> {
        self.compilers.last_mut().expect("compiler stack empty")
    }

    fn get_function_mut(&mut self) -> &mut ObjFunction {
        unsafe {
            self.current_mut()
                .function
                .as_mut()
                .expect("gc function was null")
        }
    }

    fn get_function(&self) -> &ObjFunction {
        unsafe {
            self.current()
                .function
                .as_ref()
                .expect("gc function was null")
        }
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.get_function_mut().chunk
    }

    fn current_chunk(&self) -> &Chunk {
        &self.get_function().chunk
    }

    fn end_compiler(&mut self) -> Compiler<'input> {
        self.emit_return();

        if cfg!(feature = "debug_trace") && self.parser.had_error {
            let name = unsafe { self.current().function.as_ref() }
                .and_then(|f| f.name())
                .unwrap_or("<script>");

            self.current_chunk().disassemble(name);
        }

        self.compilers.pop().expect("popped empty compiler stack")
    }

    fn begin_scope(&mut self) {
        self.current_mut().scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.current_mut().scope_depth -= 1;

        while self.current().local_count > 0
            && self.current().locals[self.current().local_count - 1].depth
                > self.current().scope_depth
        {
            if self.current().locals[self.current().local_count - 1].is_captured
            {
                self.emit_byte(Opcode::CloseUpvalue)
            } else {
                self.emit_byte(Opcode::Pop);
            }

            self.current_mut().local_count -= 1;
        }
    }

    fn emit_byte(&mut self, byte: impl Into<u8>) {
        let line = self.parser.previous.line;
        self.current_chunk_mut().write(byte, line);
    }

    fn emit_return(&mut self) {
        self.emit_byte(Opcode::Nil);
        self.emit_byte(Opcode::Return);
    }

    fn emit_bytes(&mut self, byte1: impl Into<u8>, byte2: impl Into<u8>) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_constant(&mut self, value: Value) {
        let index = self.make_constant(value);
        self.emit_bytes(Opcode::Constant, index)
    }

    /// Returns an offset in the chunk where we can put a jump distance later.
    fn emit_jump(&mut self, instruction: Opcode) -> usize {
        self.emit_byte(instruction);
        self.emit_byte(0xFF);
        self.emit_byte(0xFF);
        self.current_chunk().code.len() - 2
    }

    /// These are always backwards jumps
    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(Opcode::Loop);

        let offset = self.current_chunk_mut().code.len() - loop_start + 2;
        if offset > u16::MAX as usize {
            self.parser.error("Loop body too large.");
        }

        let [first, second] = u16::to_ne_bytes(offset as _);
        self.emit_byte(first);
        self.emit_byte(second);
    }

    fn patch_jump(&mut self, offset: usize) {
        let len = self.current_chunk_mut().code.len();

        debug_assert!(len >= offset + 2);
        let jump = len - offset - 2;

        if jump > u16::MAX as _ {
            self.parser.error("Too much code to jump over.");
        }

        let [first, second] = u16::to_ne_bytes(jump as _);
        self.current_chunk_mut().code[offset as usize] = first;
        self.current_chunk_mut().code[(offset + 1) as usize] = second;
    }

    fn make_constant(&mut self, value: impl Into<Value>) -> u8 {
        let constant = self.current_chunk_mut().add_constant(value.into());
        if constant > u8::MAX as usize {
            // It seems weird that the compiler would put an error into the
            // parser here, over a non-parser concern.
            self.parser.error("Too many constants in one chunk.");
            0
        } else {
            constant as u8
        }
    }

    fn identifier_constant(&mut self, token: Token) -> u8 {
        let name = ObjString::copy(token.lexeme, &mut self.heap);
        self.make_constant(name)
    }

    fn matches(&mut self, kind: Kind) -> bool {
        if !self.check(kind) {
            false
        } else {
            self.parser.advance();
            true
        }
    }

    fn check(&self, kind: Kind) -> bool {
        self.parser.current.kind == kind
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.parser.advance();
        let kind = self.parser.previous.kind;
        let prefix_rule = Rule::get(kind).prefix();

        if prefix_rule == None {
            self.parser.error("Expect expression.");
            return;
        }

        let can_assign = precedence <= Precedence::Assignment;
        prefix_rule.unwrap()(self, can_assign);

        while precedence <= Rule::get(self.parser.current.kind).precedence() {
            self.parser.advance();
            let previous_precedence = self.parser.previous.kind;
            let infix_rule = Rule::get(previous_precedence).infix();

            infix_rule.unwrap()(self, can_assign);
        }

        if can_assign && self.matches(Kind::Equal) {
            self.parser.error("Invalid assignment target.");
        }
    }
}

// parse/visit methods
impl<'input, 'heap> Context<'input, 'heap> {
    fn declaration(&mut self) {
        if self.matches(Kind::Class) {
            self.class_declaration();
        } else if self.matches(Kind::Fun) {
            self.fun_declaration();
        } else if self.matches(Kind::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.parser.panic_mode {
            self.parser.synchronize();
        }
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name.");
        self.current_mut().mark_initialized();
        self.function();
        self.define_variable(global);
    }

    fn function(&mut self) {
        let name = self.parser.previous.lexeme;
        let compiler = Compiler::new(name, &mut self.heap);
        self.compilers.push(compiler);

        self.begin_scope();
        self.parser
            .consume(Kind::LeftParen, "Expect '(' after function name.");

        if !self.check(Kind::RightParen) {
            // no do-while so we loop and break on condition at the end.
            loop {
                self.get_function_mut().arity += 1;

                if self.get_function_mut().arity > u8::MAX as usize {
                    self.parser.error_at_current(
                        "Can't have more than 255 parameters.",
                    );
                }

                let constant = self.parse_variable("Expect parameter name.");
                self.define_variable(constant);

                // This is the loop condition.
                if !self.matches(Kind::Comma) {
                    break;
                }
            }
        }

        self.parser
            .consume(Kind::RightParen, "Expect ')' after parameters.");
        self.parser
            .consume(Kind::LeftBrace, "Expect '{' before function body.");
        self.block();

        // we need the whole compiler back instead of just the function, since
        // we pushed it into self.compilers and need ownership back.
        let compiler = self.end_compiler();
        let function = compiler.function;

        let constant = self.make_constant(function);
        self.emit_bytes(Opcode::Closure, constant);

        let function = unsafe { function.as_ref().unwrap() };
        for i in 0..function.upvalue_count {
            let upvalue = compiler.up_values[i];
            self.emit_byte(upvalue.is_local as u8);
            self.emit_byte(upvalue.index);
        }
    }

    fn class_declaration(&mut self) {
        self.parser.consume(Kind::Identifier, "Expect class name.");
        let name_constant = self.identifier_constant(self.parser.previous);
        self.declare_variable();

        self.emit_bytes(Opcode::Class, name_constant);
        self.define_variable(name_constant);

        self.parser
            .consume(Kind::LeftBrace, "Expect '{' before class body.");
        self.parser
            .consume(Kind::RightBrace, "Expect '}' after class body.");
    }

    fn parse_variable(&mut self, error_message: &'static str) -> u8 {
        self.parser.consume(Kind::Identifier, error_message);

        self.declare_variable();

        if self.current().scope_depth > 0 {
            return 0;
        }

        self.identifier_constant(self.parser.previous)
    }

    fn define_variable(&mut self, global: u8) {
        if self.current().scope_depth > 0 {
            self.current_mut().mark_initialized();
            // Just leave the local on the stack.
            return;
        }

        self.emit_bytes(Opcode::DefineGlobal, global);
    }

    fn declare_variable(&mut self) {
        if self.current().scope_depth == 0 {
            return;
        }

        let name = self.parser.previous;

        for i in (0..self.current().local_count).rev() {
            let local = self.current().locals[i];
            if local.depth != Local::ERROR_DEPTH
                && local.depth < self.current().scope_depth
            {
                break;
            }

            if name.lexeme == local.name.lexeme {
                self.parser
                    .error("Already a variable with this name in this scope.");
            }
        }

        self.add_local(name);
    }

    fn add_local(&mut self, name: Token<'input>) {
        if self.current().local_count == u8::MAX as usize {
            self.parser.error("Too many local variables in function.");
            return;
        }

        let local_count = self.current().local_count;
        self.current_mut().locals[local_count] = Local {
            name,
            depth: Local::ERROR_DEPTH, // fixed later by mark_initialize, so we can't use a value while declaring it.
            is_captured: false,
        };
        self.current_mut().local_count += 1;
    }

    fn add_upvalue(
        &mut self,
        compiler: usize,
        index: u8,
        is_local: bool,
    ) -> u8 {
        let compiler = &mut self.compilers[compiler];
        let upvalue_count = compiler.function().upvalue_count;

        for i in 0..upvalue_count {
            let upvalue = &compiler.up_values[i];
            if upvalue.index == index && upvalue.is_local == is_local {
                return i as u8;
            }
        }

        if upvalue_count > u8::MAX as usize {
            self.parser.error("Too many closure variables in function.");
            return 0;
        }

        compiler.up_values[upvalue_count] = Upvalue { is_local, index };
        compiler.function_mut().upvalue_count += 1;
        upvalue_count as u8
    }

    fn resolve_local(&mut self, compiler: usize, name: &Token) -> Option<u8> {
        let locals = &self.compilers[compiler].locals;
        let local_count = self.compilers[compiler].local_count;

        for i in (0..local_count).rev() {
            let local = locals[i];
            if name.lexeme == local.name.lexeme {
                if local.depth == Local::ERROR_DEPTH {
                    self.parser.error(
                        "Can't read local variable in its own initializer.",
                    );
                }
                return Some(i as u8);
            }
        }

        None
    }

    fn resolve_upvalue(&mut self, compiler: usize, name: &Token) -> Option<u8> {
        if compiler == 0 {
            return None;
        }

        let enclosing = compiler - 1;

        if let Some(local) = self.resolve_local(enclosing, name) {
            self.compilers[enclosing].locals[local as usize].is_captured = true;
            return Some(self.add_upvalue(compiler, local, true));
        }

        if let Some(upvalue) = self.resolve_upvalue(enclosing, name) {
            return Some(self.add_upvalue(compiler, upvalue, false));
        }

        None
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.matches(Kind::Equal) {
            self.expression();
        } else {
            self.emit_byte(Opcode::Nil);
        }

        self.parser
            .consume(Kind::Semicolon, "Expect ';' after variable declaration.");

        self.define_variable(global);
    }

    fn block(&mut self) {
        while !self.check(Kind::RightBrace) && !self.check(Kind::Eof) {
            self.declaration();
        }
        self.parser
            .consume(Kind::RightBrace, "Expect '}' after block.");
    }

    fn statement(&mut self) {
        if self.matches(Kind::Print) {
            self.print_statement();
        } else if self.matches(Kind::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if self.matches(Kind::If) {
            self.if_statement();
        } else if self.matches(Kind::While) {
            self.while_statement();
        } else if self.matches(Kind::For) {
            self.for_statement();
        } else if self.matches(Kind::Return) {
            self.return_statement();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.parser
            .consume(Kind::Semicolon, "Expect ';' after value.");
        self.emit_byte(Opcode::Print);
    }

    fn if_statement(&mut self) {
        self.parser
            .consume(Kind::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.parser
            .consume(Kind::RightParen, "Expect ')' after 'if'.");

        let then_jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_byte(Opcode::Pop);
        self.statement();

        let else_jump = self.emit_jump(Opcode::Jump);

        self.patch_jump(then_jump);
        self.emit_byte(Opcode::Pop);

        if self.matches(Kind::Else) {
            self.statement()
        }

        self.patch_jump(else_jump)
    }

    fn while_statement(&mut self) {
        let loop_start = self.current_chunk_mut().code.len();

        self.parser
            .consume(Kind::LeftParen, "Expect '(' after 'while'");
        self.expression();
        self.parser
            .consume(Kind::RightParen, "Expect ')' after condition");

        let exit_jump = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_byte(Opcode::Pop);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_byte(Opcode::Pop);
    }

    fn for_statement(&mut self) {
        // oh boy
        self.begin_scope();
        self.parser
            .consume(Kind::LeftParen, "Expect '(' after 'while'");

        if self.matches(Kind::Semicolon) {
            // no initializer
        } else if self.matches(Kind::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.current_chunk_mut().code.len();

        let mut exit_jump: isize = -1;

        if !self.matches(Kind::Semicolon) {
            self.expression();
            self.parser
                .consume(Kind::Semicolon, "Expect ';' after loop condition");

            exit_jump = self.emit_jump(Opcode::JumpIfFalse) as _;
            self.emit_byte(Opcode::Pop);
        }

        if !self.matches(Kind::RightParen) {
            let body_jump = self.emit_jump(Opcode::Jump);
            let increment_start = self.current_chunk_mut().code.len();
            self.expression();
            self.emit_byte(Opcode::Pop);
            self.parser
                .consume(Kind::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);

        if exit_jump != -1 {
            self.patch_jump(exit_jump as _);
            self.emit_byte(Opcode::Pop);
        }

        self.end_scope();
    }

    fn return_statement(&mut self) {
        if let FunctionKind::Script = self.current().kind {
            self.parser.error("Can't return from top-level code.");
        }

        if self.matches(Kind::Semicolon) {
            self.emit_return();
        } else {
            self.expression();
            self.parser
                .consume(Kind::Semicolon, "Expect ';' after return value.");
            self.emit_byte(Opcode::Return);
        }
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.parser
            .consume(Kind::Semicolon, "Expect ';' after expression.");
        self.emit_byte(Opcode::Pop);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    pub fn number(&mut self, _can_assign: bool) {
        let number: f64 = str::parse(self.parser.previous.lexeme)
            .expect("The scanner only allows lexically-correct numbers.");
        let value = Value::from(number);
        self.emit_constant(value);
    }

    pub fn and(&mut self, _can_assign: bool) {
        let end_jump = self.emit_jump(Opcode::JumpIfFalse);

        self.emit_byte(Opcode::Pop);
        self.parse_precedence(Precedence::And);

        self.patch_jump(end_jump);
    }

    pub fn or(&mut self, _can_assign: bool) {
        let else_jump = self.emit_jump(Opcode::JumpIfFalse);
        let end_jump = self.emit_jump(Opcode::Jump);

        self.patch_jump(else_jump);
        self.emit_byte(Opcode::Pop);

        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    pub fn string(&mut self, _can_assign: bool) {
        let buf = {
            let body = self.parser.previous.lexeme;
            body[1..body.len() - 1].to_owned()
        };

        let obj_string = ObjString::make(buf, &mut self.heap);
        self.emit_constant(Value::from(obj_string))
    }

    pub fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.parser.previous, can_assign);
    }

    fn named_variable(&mut self, name: Token, can_assign: bool) {
        let current = self.compilers.len() - 1;

        let (get, set, arg) =
            if let Some(arg) = self.resolve_local(current, &name) {
                (Opcode::GetLocal, Opcode::SetLocal, arg)
            } else if let Some(arg) = self.resolve_upvalue(current, &name) {
                (Opcode::GetUpvalue, Opcode::SetUpvalue, arg)
            } else {
                let arg = self.identifier_constant(name);
                (Opcode::GetGlobal, Opcode::SetGlobal, arg)
            };

        if can_assign && self.matches(Kind::Equal) {
            self.expression();
            self.emit_bytes(set, arg);
        } else {
            self.emit_bytes(get, arg);
        };
    }

    pub fn literal(&mut self, _can_assign: bool) {
        match self.parser.previous.kind {
            Kind::False => self.emit_byte(Opcode::False),
            Kind::Nil => self.emit_byte(Opcode::Nil),
            Kind::True => self.emit_byte(Opcode::True),
            _ => {}
        }
    }

    pub fn group(&mut self, _can_assign: bool) {
        self.expression();
        self.parser
            .consume(Kind::RightParen, "Expect ')' after expression.");
    }

    pub fn call(&mut self, _can_assign: bool) {
        let argument_count = self.argument_list();

        self.emit_bytes(Opcode::Call, argument_count);
    }

    pub fn dot(&mut self, can_assign: bool) {
        self.parser
            .consume(Kind::Identifier, "Expect property name after '.'.");
        let name = self.identifier_constant(self.parser.previous);

        if can_assign && self.matches(Kind::Equal) {
            self.expression();
            self.emit_bytes(Opcode::SetProperty, name);
        } else {
            self.emit_bytes(Opcode::GetProperty, name);
        }
    }

    fn argument_list(&mut self) -> u8 {
        let mut arg_count = 0;

        if !self.check(Kind::RightParen) {
            loop {
                self.expression();
                if arg_count == u8::MAX {
                    self.parser.error("Can't have more than 255 arguments.");
                }
                arg_count += 1;

                // Replicate the do-while with a if-break at the end.
                if !self.matches(Kind::Comma) {
                    break;
                }
            }
        }

        self.parser
            .consume(Kind::RightParen, "Expect ')' after arguments.");

        arg_count
    }

    pub fn unary(&mut self, _can_assign: bool) {
        let operator_kind = self.parser.previous.kind;

        self.parse_precedence(Precedence::Unary);

        match operator_kind {
            Kind::Bang => self.emit_byte(Opcode::Not),
            Kind::Minus => self.emit_byte(Opcode::Negate),
            _ => unreachable!(),
        }
    }

    pub fn binary(&mut self, _can_assign: bool) {
        let operator_kind = self.parser.previous.kind;
        let rule = Rule::get(operator_kind);

        self.parse_precedence(rule.precedence().next());

        {
            use Kind as K;
            use Opcode::*;
            match operator_kind {
                K::BangEqual => self.emit_bytes(Equal, Not),
                K::EqualEqual => self.emit_byte(Equal),
                K::Greater => self.emit_byte(Greater),
                K::GreaterEqual => self.emit_bytes(Less, Not),
                K::Less => self.emit_byte(Less),
                K::LessEqual => self.emit_bytes(Greater, Not),
                K::Plus => self.emit_byte(Add),
                K::Minus => self.emit_byte(Sub),
                K::Star => self.emit_byte(Mul),
                K::Slash => self.emit_byte(Div),
                _ => unreachable!(),
            }
        }
    }
}

pub struct Compiler<'input> {
    function: *mut ObjFunction,
    kind: FunctionKind,
    locals: [Local<'input>; Local::MAX_LOCALS],
    local_count: usize,
    up_values: [Upvalue; ObjClosure::MAX_UPVALUES],
    scope_depth: isize,
}

impl<'input> Compiler<'input> {
    fn new_top_level(heap: &mut VM) -> Self {
        let function = ObjFunction::new(heap);

        Compiler {
            kind: FunctionKind::Script,
            function,
            locals: [Local::RETURN_PLACEHOLDER; Local::MAX_LOCALS],
            local_count: 0,
            up_values: [Upvalue::UNINIT; ObjClosure::MAX_UPVALUES],
            scope_depth: 0,
        }
    }

    fn function(&self) -> &ObjFunction {
        unsafe { (*self).function.as_ref().unwrap() }
    }

    fn function_mut(&mut self) -> &mut ObjFunction {
        unsafe { (*self).function.as_mut().unwrap() }
    }

    fn new(name: &str, heap: &mut VM) -> Self {
        let function = ObjFunction::new(heap);
        unsafe { (*function).name = ObjString::copy(name, heap) };

        Compiler {
            function,
            kind: FunctionKind::Function,
            locals: [Local::RETURN_PLACEHOLDER; Local::MAX_LOCALS],
            local_count: 0,
            up_values: [Upvalue::UNINIT; ObjClosure::MAX_UPVALUES],
            scope_depth: 0,
        }
    }

    fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            return;
        }

        self.locals[self.local_count - 1].depth = self.scope_depth;
    }
}

#[derive(Clone, Copy, Debug)]
struct Local<'a> {
    name: Token<'a>,
    depth: isize,
    is_captured: bool,
}

impl Local<'_> {
    const RETURN_PLACEHOLDER: Local<'static> = Local {
        name: Token::fake(),
        depth: Local::ERROR_DEPTH,
        is_captured: false,
    };

    const MAX_LOCALS: usize = u8::MAX as usize;
    const ERROR_DEPTH: isize = -1;
}

#[derive(PartialEq, Clone, Copy)]
struct Upvalue {
    is_local: bool,
    index: u8,
}

impl Upvalue {
    pub const UNINIT: Upvalue = Upvalue {
        is_local: true,
        index: 0,
    };
}
