use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    object::{Obj, ObjType, Object},
    value::Value,
    vm::VM,
};

impl Object for ObjNative {
    const TAG: ObjType = ObjType::Native;
}

pub type NativeFn = fn(vm: &mut VM) -> Value;

#[repr(C)]
pub struct ObjNative {
    obj: Obj,
    pub function: NativeFn,
}

impl ObjNative {
    pub fn new(function: NativeFn, heap: &mut VM) -> *mut ObjNative {
        unsafe { heap.allocate(|obj| ObjNative { obj, function }) }
    }
}

pub fn native_clock(_vm: &mut VM) -> Value {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs_f64()
        .into()
}
