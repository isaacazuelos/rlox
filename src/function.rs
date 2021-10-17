use std::{
    fmt::{self, Formatter},
    ptr::null_mut,
};

use crate::{
    chunk::Chunk,
    object::{Obj, ObjType, Object},
    string::ObjString,
    vm::VM,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FunctionKind {
    Function,
    Script,
    Method,
    Initializer,
}

#[repr(C, align(16))]
pub struct ObjFunction {
    obj: Obj,
    pub arity: usize,
    pub upvalue_count: usize,
    pub chunk: Chunk,
    pub name: *mut ObjString,
}

impl Object for ObjFunction {
    const TAG: ObjType = ObjType::Function;
}

impl std::fmt::Debug for ObjFunction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.name() {
            Some(name) => write!(f, "<fn {}>", name),
            None => write!(f, "<fn>"),
        }
    }
}

impl ObjFunction {
    pub fn new(heap: &mut VM) -> *mut ObjFunction {
        unsafe {
            heap.allocate(|obj| ObjFunction {
                obj,
                arity: 0,
                upvalue_count: 0,
                name: null_mut(),
                chunk: Chunk::new(),
            })
        }
    }

    pub fn name(&self) -> Option<&str> {
        unsafe { self.name.as_ref().map(|o| o.as_str()) }
    }
}
