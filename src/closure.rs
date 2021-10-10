use std::{
    fmt::{self, Debug, Formatter},
    ptr::null_mut,
};

use crate::{
    function::ObjFunction,
    object::{Obj, ObjType, Object},
    upvalue::ObjUpvalue,
    vm::VM,
};

#[repr(C, align(16))]
pub struct ObjClosure {
    obj: Obj,
    pub function: *mut ObjFunction,
    pub upvalues: [*mut ObjUpvalue; ObjClosure::MAX_UPVALUES],
    pub upvalue_count: usize,
}

impl Object for ObjClosure {
    const TAG: ObjType = ObjType::Closure;
}

impl ObjClosure {
    pub const MAX_UPVALUES: usize = u8::MAX as usize + 1;

    pub fn new(function: *mut ObjFunction, heap: &mut VM) -> *mut ObjClosure {
        unsafe {
            let upvalue_count = function
                .as_ref()
                .expect("cannot make closure from null function")
                .upvalue_count;

            heap.allocate(|obj| ObjClosure {
                obj,
                function,
                upvalues: [null_mut(); ObjClosure::MAX_UPVALUES],
                upvalue_count,
            })
        }
    }

    pub fn function(&self) -> &ObjFunction {
        unsafe { &(*self.function) }
    }

    pub fn function_mut(&mut self) -> &mut ObjFunction {
        unsafe { &mut (*self.function) }
    }
}

impl Debug for ObjClosure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<closure {} upvalues: {:?}>",
            self.function().name().unwrap_or("<anonymous>"),
            self.upvalues[0..self.upvalue_count]
                .iter()
                .map(|u| unsafe {
                    u.as_mut()
                        .map(|u| format!("{}", u.get()))
                        .unwrap_or_else(|| "<null ptr>".into())
                })
                .collect::<Vec<_>>()
        )
    }
}
