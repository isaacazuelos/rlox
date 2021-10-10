use std::{
    cell::RefCell,
    collections::HashMap,
    mem::size_of,
    ptr::null_mut,
    rc::{Rc, Weak},
};

use crate::object::{Obj, Object};

pub struct VM {
    head: *mut Obj,
    /// Interning pool, owns the backing buffers as strings. These strings must
    /// not be mutated or the interior `*const` pointers we use in
    /// [`ObjString`][crate::string::ObjString] will break
    ///
    /// The hashmap moving it's buffer doesn't move the strings, so it's okay to
    /// use those interior `*const` pointers if the actual strings don't mutate.
    strings: HashMap<u64, String>,
}

impl VM {}

impl VM {}
