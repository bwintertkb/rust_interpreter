use std::{collections::HashMap, rc::Rc};

use once_cell::sync::Lazy;

use crate::{
    ast::{BlockStatement, Identifier},
    environment::Environment,
};

pub const INTEGER_OBJ: &str = "INTEGER";
pub const BOOLEAN_OBJ: &str = "BOOLEAN";
pub const NULL_OBJ: &str = "NULL";
pub const RETURN_VALUE_OBJ: &str = "RETURN_VALUE";
pub const ERROR_OBJ: &str = "ERROR";
pub const FUNCTION_OBJ: &str = "FUNCTION";
pub const STRING_OBJ: &str = "STRING";
pub const BUILTIN_OBJ: &str = "BUILTIN";
pub const ARRAY_OBJ: &str = "ARRAY";

pub static NULL: Lazy<Null> = Lazy::new(|| Null {});

pub type ObjectType = &'static str;

pub static BUILTINS: Lazy<HashMap<String, BuiltinFunction>> = Lazy::new(|| {
    let mut builtins = HashMap::new();
    builtins.insert("len".to_owned(), BuiltinFunction::new(builtin_len));

    builtins
});

fn builtin_len(args: Vec<Objects>) -> Objects {
    if args.len() != 1 {
        return Objects::Error(ErrorMonkey::new(&format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }
    match args[0] {
        Objects::String(ref s) => Objects::Integer(Integer::new(s.value.len() as i64)),
        _ => Objects::Error(ErrorMonkey::new(&format!(
            "argument to `len` not supported, got {}",
            args[0].obj_type()
        ))),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Objects {
    Integer(Integer),
    Boolean(&'static Boolean),
    ReturnValue(Box<ReturnValue>),
    Null(&'static Null),
    Error(ErrorMonkey),
    Function(Function),
    String(StringObject),
    Builtin(BuiltinFunction),
    Array(Array),
}

impl Objects {
    pub fn is_null(&self) -> bool {
        matches!(self, Objects::Null(_))
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Objects::Error(_))
    }

    pub fn is_return(&self) -> bool {
        matches!(self, Objects::ReturnValue(_))
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Objects::Function(_))
    }
}

impl Object for Objects {
    fn obj_type(&self) -> ObjectType {
        match self {
            Objects::Integer(i) => i.obj_type(),
            Objects::Boolean(b) => b.obj_type(),
            Objects::Null(n) => n.obj_type(),
            Objects::ReturnValue(r) => r.obj_type(),
            Objects::Error(e) => e.obj_type(),
            Objects::Function(f) => f.obj_type(),
            Objects::String(s) => s.obj_type(),
            Objects::Builtin(b) => b.obj_type(),
            Objects::Array(a) => a.obj_type(),
        }
    }

    fn inspect(&self) -> String {
        match self {
            Objects::Integer(i) => i.inspect(),
            Objects::Boolean(b) => b.inspect(),
            Objects::Null(n) => n.inspect(),
            Objects::ReturnValue(r) => r.inspect(),
            Objects::Error(e) => e.inspect(),
            Objects::Function(f) => f.inspect(),
            Objects::String(s) => s.inspect(),
            Objects::Builtin(b) => b.inspect(),
            Objects::Array(a) => a.inspect(),
        }
    }
}

pub trait Object {
    fn obj_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Array {
    pub elements: Vec<Objects>,
}

impl Array {
    pub fn new(elements: Vec<Objects>) -> Self {
        Array { elements }
    }
}

impl Object for Array {
    fn obj_type(&self) -> ObjectType {
        ARRAY_OBJ
    }

    fn inspect(&self) -> String {
        let mut s = String::new();
        s.push('[');
        for (i, e) in self.elements.iter().enumerate() {
            s.push_str(&e.inspect());
            if i != self.elements.len() - 1 {
                s.push_str(", ");
            }
        }
        s.push(']');
        s
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringObject {
    pub value: String,
}

impl StringObject {
    pub fn new(value: &str) -> Self {
        StringObject {
            value: value.to_owned(),
        }
    }
}

impl Object for StringObject {
    fn obj_type(&self) -> ObjectType {
        STRING_OBJ
    }

    fn inspect(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Null;

impl Object for Null {
    fn obj_type(&self) -> ObjectType {
        NULL_OBJ
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReturnValue {
    pub value: Objects,
}

impl ReturnValue {
    pub fn new(value: Objects) -> Self {
        ReturnValue { value }
    }
}

impl Object for ReturnValue {
    fn obj_type(&self) -> ObjectType {
        RETURN_VALUE_OBJ
    }

    fn inspect(&self) -> String {
        self.value.inspect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ErrorMonkey {
    pub message: String,
}

impl ErrorMonkey {
    pub fn new(message: &str) -> Self {
        ErrorMonkey {
            message: message.to_owned(),
        }
    }
}

impl Object for ErrorMonkey {
    fn obj_type(&self) -> ObjectType {
        ERROR_OBJ
    }

    fn inspect(&self) -> String {
        format!("ERROR: {}", self.message)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<Environment>,
}

impl Function {
    pub fn new(parameters: Vec<Identifier>, body: BlockStatement, env: Rc<Environment>) -> Self {
        Function {
            parameters,
            body,
            env,
        }
    }
}

impl Object for Function {
    fn obj_type(&self) -> ObjectType {
        FUNCTION_OBJ
    }

    fn inspect(&self) -> String {
        let mut params = Vec::new();
        for p in &self.parameters {
            params.push(p.string());
        }
        format!("fn({}) {{\n{}\n}}", params.join(", "), self.body.string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BuiltinFunction {
    pub func: fn(Vec<Objects>) -> Objects,
}

impl BuiltinFunction {
    pub fn new(func: fn(Vec<Objects>) -> Objects) -> Self {
        BuiltinFunction { func }
    }
}

impl Object for BuiltinFunction {
    fn obj_type(&self) -> ObjectType {
        BUILTIN_OBJ
    }

    fn inspect(&self) -> String {
        "builtin function".to_string()
    }
}
