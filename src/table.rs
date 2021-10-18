use std::collections::HashMap;

use crate::{string::ObjString, value::Value};

#[derive(Debug)]
pub struct Table {
    pub entries: HashMap<u64, (*const ObjString, Value)>,
}

impl Table {
    pub fn new() -> Self {
        Table {
            entries: HashMap::new(),
        }
    }

    pub fn set(&mut self, key: *const ObjString, value: Value) -> bool {
        // println!("table.set({}, {})", hash(key), value);
        self.entries.insert(hash(key), (key, value)).is_none()
    }

    pub fn get(&self, key: *const ObjString) -> Option<Value> {
        let value = self.entries.get(&hash(key)).map(|(_, v)| *v);
        // println!("table.get({}) = {:?}", hash(key), value);
        value
    }

    pub fn add_all(&mut self, other: &Table) {
        self.entries.extend(other.entries.iter());
    }

    pub fn delete(&mut self, key: *const ObjString) -> bool {
        self.entries.remove(&hash(key)).is_some()
    }

    pub fn remove_white_strings(&mut self) {
        let mut to_remove = Vec::new();

        for (h, (_, v)) in &self.entries {
            if v.is_obj() {
                if !v.as_obj().is_marked {
                    to_remove.push(*h);
                }
            }
        }

        for hash in to_remove {
            self.entries.remove(&hash);
        }
    }
}

fn hash(key: *const ObjString) -> u64 {
    let s = unsafe { key.as_ref() }.expect("non-null string key");
    s.hash()
}
