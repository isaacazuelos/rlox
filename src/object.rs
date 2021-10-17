use std::fmt::{self, Display, Formatter};

use crate::{
    class::ObjClass,
    closure::ObjClosure,
    function::ObjFunction,
    instance::{ObjBoundMethod, ObjInstance},
    native::ObjNative,
    string::ObjString,
    upvalue::ObjUpvalue,
};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ObjType {
    BoundMethod,
    Class,
    Closure,
    Function,
    Instance,
    Native,
    String,
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
            ObjType::Class => {
                let class = object as *mut ObjClass;
                drop(Box::from_raw(class))
            }
            ObjType::Instance => {
                let instance = object as *mut ObjInstance;
                drop(Box::from_raw(instance))
            }
            ObjType::BoundMethod => {
                let bm = object as *mut ObjBoundMethod;
                drop(Box::from_raw(bm))
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
            ObjType::Class => unsafe {
                let outer: &ObjClass = std::mem::transmute(self);
                let name = outer.name.as_ref().unwrap().as_str();
                write!(f, "{}", name)
            },
            ObjType::Instance => unsafe {
                let outer: &ObjInstance = std::mem::transmute(self);
                let class = outer.class.as_ref().unwrap();
                let name = class.name.as_ref().unwrap().as_str();
                write!(f, "{} instance", name)
            },
            ObjType::BoundMethod => unsafe {
                let outer: &ObjBoundMethod = std::mem::transmute(self);
                let method: &ObjClosure = outer.method.as_mut().unwrap();
                let function: &ObjFunction = method.function();
                write!(f, "{:?}", function)
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
            ObjType::Upvalue => unsafe {
                let l: &ObjUpvalue = std::mem::transmute(self);
                let r: &ObjUpvalue = std::mem::transmute(other);
                l.get() == r.get()
            },

            ObjType::Closure
            | ObjType::Function
            | ObjType::Instance // good enough for now, I think.
            | ObjType::Native
            | ObjType::BoundMethod
            | ObjType::Class => std::ptr::eq(self, other),
        }
    }
}

pub trait Object {
    const TAG: ObjType;
}
