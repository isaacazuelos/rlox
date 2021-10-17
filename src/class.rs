use crate::{
    object::{Obj, ObjType, Object},
    string::ObjString,
    table::Table,
    vm::VM,
};

#[repr(C, align(16))]
pub struct ObjClass {
    pub obj: Obj,
    pub name: *mut ObjString,
    pub methods: Table,
}

impl ObjClass {
    pub fn new(name: *mut ObjString, heap: &mut VM) -> *mut ObjClass {
        unsafe {
            heap.allocate(|obj| ObjClass {
                obj,
                name,
                methods: Table::new(),
            })
        }
    }
}

impl Object for ObjClass {
    const TAG: ObjType = ObjType::Class;
}
