use std::any::Any;

use once_cell::sync::Lazy;

const INTEGER_OBJ: &str = "INTEGER";
const BOOLEAN_OBJ: &str = "BOOLEAN";
const NULL_OBJ: &str = "NULL";

pub static NULL: Lazy<Null> = Lazy::new(|| Null {});

pub type ObjectType = &'static str;

#[derive(Debug, Clone)]
pub enum Objects {
    Integer(Integer),
    Boolean(&'static Boolean),
    Null(&'static Null),
}

impl Objects {
    pub fn string(&self) -> String {
        match self {
            Objects::Integer(i) => i.inspect(),
            Objects::Boolean(b) => b.inspect(),
            Objects::Null(n) => n.inspect(),
        }
    }
}

pub trait Object {
    fn obj_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug, Clone, Copy)]
pub struct Integer {
    pub value: i64,
}

impl Integer {
    pub fn new(value: i64) -> Self {
        Integer { value }
    }
}

impl Object for Integer {
    fn obj_type(&self) -> ObjectType {
        INTEGER_OBJ
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Boolean {
    pub value: bool,
}

impl Boolean {
    pub fn new(value: bool) -> Self {
        Boolean { value }
    }
}

impl Object for Boolean {
    fn obj_type(&self) -> ObjectType {
        BOOLEAN_OBJ
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Null;

impl Object for Null {
    fn obj_type(&self) -> ObjectType {
        NULL_OBJ
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}
