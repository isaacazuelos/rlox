use crate::{
    class::ObjClass,
    closure::ObjClosure,
    object::{Obj, ObjType, Object},
    table::Table,
    value::Value,
    vm::VM,
};

#[repr(C, align(16))]
pub struct ObjInstance {
    pub obj: Obj,
    pub class: *mut ObjClass,
    pub fields: Table,
}

impl ObjInstance {
    pub fn new(class: *mut ObjClass, heap: &mut VM) -> *mut ObjInstance {
        unsafe {
            heap.allocate(|obj| ObjInstance {
                obj,
                class,
                fields: Table::new(),
            })
        }
    }
}

impl Object for ObjInstance {
    const TAG: ObjType = ObjType::Instance;
}

#[repr(C, align(16))]
pub struct ObjBoundMethod {
    pub obj: Obj,
    pub receiver: Value,
    pub method: *mut ObjClosure,
}

impl Object for ObjBoundMethod {
    const TAG: ObjType = ObjType::BoundMethod;
}

impl ObjBoundMethod {
    pub fn new(
        receiver: Value,
        method: *mut ObjClosure,
        heap: &mut VM,
    ) -> *mut ObjBoundMethod {
        unsafe {
            heap.allocate(|obj| ObjBoundMethod {
                obj,
                method,
                receiver,
            })
        }
    }
}
