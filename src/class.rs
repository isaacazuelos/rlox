use crate::{
    object::{Obj, ObjType, Object},
    string::ObjString,
    vm::VM,
};

#[repr(C, align(16))]
pub struct ObjClass {
    pub obj: Obj,
    pub name: *mut ObjString,
}

impl ObjClass {
    pub fn new(name: *mut ObjString, heap: &mut VM) -> *mut ObjClass {
        unsafe { heap.allocate(|obj| ObjClass { obj, name }) }
    }
}

impl Object for ObjClass {
    const TAG: ObjType = ObjType::Class;
}
