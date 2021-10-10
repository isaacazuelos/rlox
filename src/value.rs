use std::{
    fmt::{self, Debug, Display, Formatter},
    ops::{Div, Mul, Sub},
};

use crate::{
    object::{Obj, ObjType, Object},
    string::ObjString,
    vm::VM,
};

#[derive(Debug)]
pub enum TypeError {
    Numbers,
    AddMismatch,
}

#[derive(Clone, Copy)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    Obj(*mut Obj),
}

impl Default for Value {
    fn default() -> Value {
        Value::Nil
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{}", n),
            Value::Obj(_) => write!(f, "{}", self.as_obj()),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        use Value::*;
        match (self, other) {
            (Nil, Nil) => true,
            (Boolean(l0), Boolean(r0)) => l0 == r0,
            (Number(l0), Number(r0)) => l0 == r0,
            (Obj(lhs), Obj(rhs)) => {
                let l = self.as_obj();
                let r = other.as_obj();
                match (l.obj_type(), r.obj_type()) {
                    (ObjType::String, ObjType::String) => {
                        let l = l.as_a::<ObjString>().unwrap();
                        let r = r.as_a::<ObjString>().unwrap();
                        l.as_str() == r.as_str()
                    }
                    (ObjType::Closure, ObjType::Closure) => {
                        std::ptr::eq(*lhs, *rhs)
                    }
                    (_, _) => false,
                }
            }
            _ => false,
        }
    }
}

impl Value {
    pub fn add(self, other: Value, heap: &mut VM) -> Result<Self, TypeError> {
        use Value::*;
        match (self, other) {
            (Number(l), Number(r)) => Ok(Number(l + r)),
            (Obj(_), Obj(_)) => {
                let l = self.as_obj();
                let r = other.as_obj();
                match (l.obj_type(), r.obj_type()) {
                    (ObjType::String, ObjType::String) => {
                        let l = l.as_a::<ObjString>().unwrap();
                        let r = r.as_a::<ObjString>().unwrap();
                        Ok(l.concatenate(r, heap))
                    }
                    (_, _) => Err(TypeError::AddMismatch),
                }
            }
            _ => Err(TypeError::AddMismatch),
        }
    }

    pub fn greater_than(self, other: Value) -> Result<Self, TypeError> {
        use Value::{Boolean, Number};

        if let (Number(l), Number(r)) = (self, other) {
            Ok(Boolean(l > r))
        } else {
            Err(TypeError::Numbers)
        }
    }

    pub fn less_than(self, other: Value) -> Result<Self, TypeError> {
        use Value::{Boolean, Number};

        if let (Number(l), Number(r)) = (self, other) {
            Ok(Boolean(l < r))
        } else {
            Err(TypeError::Numbers)
        }
    }
}

impl Sub for Value {
    type Output = Result<Self, TypeError>;
    fn sub(self, rhs: Self) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l - r)),
            _ => Err(TypeError::Numbers),
        }
    }
}

impl Mul for Value {
    type Output = Result<Self, TypeError>;
    fn mul(self, rhs: Self) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l * r)),
            _ => Err(TypeError::Numbers),
        }
    }
}

impl Div for Value {
    type Output = Result<Self, TypeError>;
    fn div(self, rhs: Self) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l / r)),
            _ => Err(TypeError::Numbers),
        }
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Nil
    }
}

impl Value {
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Boolean(b)
    }
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        self.is_nil() || matches!(self, Value::Boolean(false))
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Number(f)
    }
}

impl Value {
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn as_number(&self) -> f64 {
        match self {
            Value::Number(b) => *b,
            _ => panic!("cannot cast {:?} as Number", self),
        }
    }
}

impl<T: Object> From<*mut T> for Value {
    fn from(ptr: *mut T) -> Self {
        Value::Obj(ptr as *mut Obj)
    }
}

impl Value {
    pub fn is_obj(&self) -> bool {
        matches!(self, Value::Obj(_))
    }

    pub fn as_obj(&self) -> &Obj {
        match self {
            Value::Obj(o) => unsafe { &**o },
            _ => panic!("cannot cast {:?} as Number", self),
        }
    }

    pub fn as_obj_mut(&mut self) -> &mut Obj {
        match self {
            Value::Obj(o) => unsafe { &mut **o },
            _ => panic!("cannot cast {:?} as Number", self),
        }
    }

    pub fn as_a<T: Object>(&self) -> Option<&T> {
        if let Value::Obj(obj) = self {
            if let Some(obj) = unsafe { obj.as_ref() } {
                return obj.as_a();
            }
        }

        None
    }

    pub fn as_a_mut<T: Object>(&mut self) -> Option<&mut T> {
        if let Value::Obj(obj) = self {
            if let Some(obj) = unsafe { obj.as_mut() } {
                return obj.as_a_mut();
            }
        }

        None
    }
}
