use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

use crate::{
    object::{Obj, ObjType, Object},
    value::Value,
    vm::VM,
};

#[repr(C, align(16))]
pub struct ObjString {
    pub obj: Obj,
    pub hash: u64,
    pub buf: String,
}

impl Object for ObjString {
    const TAG: ObjType = ObjType::String;
}

impl PartialEq for ObjString {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && self.buf == other.buf
    }
}

impl ObjString {
    fn compute_hash(s: &str) -> u64 {
        let mut h = DefaultHasher::new();
        s.hash(&mut h);
        h.finish()
    }

    pub fn as_str(&self) -> &str {
        &self.buf
    }

    pub fn hash(&self) -> u64 {
        self.hash
    }

    pub fn copy(s: &str, heap: &mut VM) -> *mut ObjString {
        let hash = ObjString::compute_hash(s);
        if let Some(buf) = heap.get_interned(hash) {
            buf
        } else {
            ObjString::make(s.into(), heap)
        }
    }

    pub fn make(buf: String, heap: &mut VM) -> *mut ObjString {
        let hash = ObjString::compute_hash(&buf);
        if let Some(interned) = heap.get_interned(hash) {
            interned
        } else {
            let obj =
                unsafe { heap.allocate(|obj| ObjString { obj, hash, buf }) };
            heap.intern(hash, obj);
            obj
        }
    }

    pub fn concatenate(&self, other: &ObjString, heap: &mut VM) -> Value {
        let mut buf = self.as_str().to_owned();
        buf.push_str(other.as_str());
        ObjString::make(buf, heap).into()
    }
}
