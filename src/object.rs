use std::fmt::{self, Display, Formatter};

use crate::{
    closure::ObjClosure, function::ObjFunction, native::ObjNative,
    string::ObjString, upvalue::ObjUpvalue,
};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ObjType {
    String,
    Function,
    Native,
    Closure,
    Upvalue,
}

#[derive(Debug)]
pub struct Obj {
    pub obj_type: ObjType,
    pub is_marked: bool,
    pub next: *mut Obj,
}

impl Obj {
    pub fn obj_type(&self) -> ObjType {
        self.obj_type
    }

    pub fn upcast_mut(obj: &mut impl Object) -> &mut Obj {
        unsafe { std::mem::transmute(obj) }
    }

    pub unsafe fn free(object: *mut Obj) {
        let tag = (*object).obj_type;

        if cfg!(feature = "debug_log_gc") {
            println!("0x{:X} free type {:?}\n", object as usize, tag);
        }

        match tag {
            ObjType::String => {
                let obj_str = object as *mut ObjString;
                drop(Box::from_raw(obj_str))
            }
            ObjType::Function => {
                let obj_fn = object as *mut ObjFunction;
                drop(Box::from_raw(obj_fn))
            }
            ObjType::Closure => {
                let obj_c = object as *mut ObjClosure;
                drop(Box::from_raw(obj_c))
            }
            ObjType::Native => {
                let native = object as *mut ObjNative;
                drop(Box::from_raw(native))
            }
            ObjType::Upvalue => {
                let upvalue = object as *mut ObjUpvalue;
                drop(Box::from_raw(upvalue))
            }
        }
    }

    pub fn as_a<T: Object>(&self) -> Option<&T> {
        if self.obj_type == T::TAG {
            unsafe { Some(std::mem::transmute(self)) }
        } else {
            None
        }
    }

    pub fn as_a_mut<T: Object>(&mut self) -> Option<&mut T> {
        if self.obj_type == T::TAG {
            unsafe { Some(std::mem::transmute(self)) }
        } else {
            None
        }
    }
}

impl Display for Obj {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.obj_type() {
            ObjType::String => unsafe {
                let inner: &ObjString = std::mem::transmute(self);
                write!(f, "{}", inner.as_str())
            },
            ObjType::Function => unsafe {
                let inner: &ObjFunction = std::mem::transmute(self);
                write!(f, "{:?}", inner)
            },
            ObjType::Closure => unsafe {
                let outer: &ObjClosure = std::mem::transmute(self);
                let inner = outer.function.as_ref().unwrap();
                write!(f, "{:?}", inner)
            },
            ObjType::Native => write!(f, "<native fn>"),
            ObjType::Upvalue => unsafe {
                let outer: &ObjUpvalue = std::mem::transmute(self);
                write!(f, "<upvalue {:?}>", outer)
            },
        }
    }
}

impl PartialEq for Obj {
    fn eq(&self, other: &Obj) -> bool {
        if self.obj_type() != other.obj_type() {
            return false;
        }
        match self.obj_type() {
            ObjType::String => unsafe {
                let l: &ObjString = std::mem::transmute(self);
                let r: &ObjString = std::mem::transmute(other);
                l == r
            },
            ObjType::Closure | ObjType::Function | ObjType::Native => {
                std::ptr::eq(self, other)
            }
            ObjType::Upvalue => unsafe {
                let l: &ObjUpvalue = std::mem::transmute(self);
                let r: &ObjUpvalue = std::mem::transmute(other);
                l.get() == r.get()
            },
        }
    }
}

pub trait Object {
    const TAG: ObjType;
}
