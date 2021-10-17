use crate::{
    class::ObjClass,
    object::{Obj, ObjType, Object},
    table::Table,
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
