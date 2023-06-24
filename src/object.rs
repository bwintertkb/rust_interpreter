use std::any::Any;

const INTEGER_OBJ: &str = "INTEGER";
const BOOLEAN_OBJ: &str = "BOOLEAN";
const NULL_OBJ: &str = "BOOLEAN";

pub type ObjectType = &'static str;

pub trait Object {
    fn obj_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn as_any(&self) -> &dyn Any
    where
        Self: 'static + Sized,
    {
        self
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Integer {
    pub value: i64,
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
