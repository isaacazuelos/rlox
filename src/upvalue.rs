use std::fmt::{self, Debug, Formatter};

use crate::{
    object::{Obj, ObjType, Object},
    value::Value,
    vm::VM,
};

#[repr(C, align(16))]
pub struct ObjUpvalue {
    obj: Obj,
    pub state: State,
}

pub enum State {
    Open(*mut Value),
    Closed(Value),
}

impl Object for ObjUpvalue {
    const TAG: ObjType = ObjType::Upvalue;
}

impl ObjUpvalue {
    pub fn new(slot: *mut Value, heap: &mut VM) -> *mut ObjUpvalue {
        unsafe {
            heap.allocate(|obj| ObjUpvalue {
                obj,
                state: State::Open(slot),
            })
        }
    }

    pub fn is_open(&self) -> bool {
        matches!(self.state, State::Open(_))
    }

    pub fn location(&mut self) -> *mut Value {
        match self.state {
            State::Open(ptr) => ptr,
            State::Closed(ref mut value) => value,
        }
    }

    pub fn get(&self) -> Value {
        match self.state {
            State::Open(ptr) => unsafe { *ptr },
            State::Closed(v) => v,
        }
    }

    pub fn set(&mut self, value: Value) {
        match self.state {
            State::Open(ptr) => unsafe { *ptr = value },
            State::Closed(ref mut val) => *val = value,
        }
    }

    pub fn close(&mut self) {
        debug_assert!(self.is_open(), "cannot close closed upvalue");
        let value = self.get();
        self.state = State::Closed(value);
    }
}

impl Debug for ObjUpvalue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<{} upvalue {:?}>",
            if self.is_open() { "open" } else { "closed " },
            self.get()
        )
    }
}
