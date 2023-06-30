use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    rc::Rc,
};

use once_cell::sync::Lazy;

use crate::{
    ast::{BlockStatement, Identifier},
    environment::Environment,
    evaluator::native_bool_to_boolean_obj,
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
pub const HASH_OBJ: &str = "HASH";

pub static NULL: Lazy<Null> = Lazy::new(|| Null {});

pub type ObjectType = &'static str;

pub static BUILTINS: Lazy<HashMap<String, BuiltinFunction>> = Lazy::new(|| {
    let mut builtins = HashMap::new();
    builtins.insert("len".to_owned(), BuiltinFunction::new(builtin_len));
    builtins.insert("first".to_owned(), BuiltinFunction::new(builtin_first));
    builtins.insert("last".to_owned(), BuiltinFunction::new(builtin_last));
    builtins.insert("rest".to_owned(), BuiltinFunction::new(builtin_rest));
    builtins.insert("push".to_owned(), BuiltinFunction::new(builtin_push));

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
        Objects::Array(ref a) => Objects::Integer(Integer::new(a.elements.len() as i64)),
        _ => Objects::Error(ErrorMonkey::new(&format!(
            "argument to `len` not supported, got {}",
            args[0].obj_type()
        ))),
    }
}

fn builtin_first(args: Vec<Objects>) -> Objects {
    if args.len() != 1 {
        return Objects::Error(ErrorMonkey::new(&format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }
    match args[0] {
        Objects::Array(ref a) => {
            if a.elements.is_empty() {
                return Objects::Null(&NULL);
            }
            a.elements[0].clone()
        }
        _ => Objects::Error(ErrorMonkey::new(&format!(
            "argument to `first` must be ARRAY, got {}",
            args[0].obj_type()
        ))),
    }
}

fn builtin_last(args: Vec<Objects>) -> Objects {
    if args.len() != 1 {
        return Objects::Error(ErrorMonkey::new(&format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }
    match args[0] {
        Objects::Array(ref a) => {
            if a.elements.is_empty() {
                return Objects::Null(&NULL);
            }
            a.elements[a.elements.len() - 1].clone()
        }
        _ => Objects::Error(ErrorMonkey::new(&format!(
            "argument to `last` must be ARRAY, got {}",
            args[0].obj_type()
        ))),
    }
}

fn builtin_rest(args: Vec<Objects>) -> Objects {
    if args.len() != 1 {
        return Objects::Error(ErrorMonkey::new(&format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }
    match args[0] {
        Objects::Array(ref a) => {
            if a.elements.is_empty() {
                return Objects::Null(&NULL);
            }
            let new_arr: Vec<Objects> = a.elements[1..].to_vec();
            Objects::Array(Array::new(new_arr))
        }
        _ => Objects::Error(ErrorMonkey::new(&format!(
            "argument to `last` must be ARRAY, got {}",
            args[0].obj_type()
        ))),
    }
}

fn builtin_push(args: Vec<Objects>) -> Objects {
    if args.len() != 2 {
        return Objects::Error(ErrorMonkey::new(&format!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        )));
    }
    match args[0] {
        Objects::Array(ref a) => {
            if a.elements.is_empty() {
                return Objects::Null(&NULL);
            }
            let mut new_arr: Vec<Objects> = a.elements[..].to_vec();
            new_arr.push(args[1].clone());
            Objects::Array(Array::new(new_arr))
        }
        _ => Objects::Error(ErrorMonkey::new(&format!(
            "argument to `last` must be ARRAY, got {}",
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
    HashMap(HashMapMonkey),
    HashKey(HashKey),
    HashPair(Box<HashPair>),
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

    pub fn hash_value(&self) -> u64 {
        match self {
            Objects::Integer(i) => i.value as u64,
            Objects::Boolean(b) => {
                if b.value {
                    1
                } else {
                    0
                }
            }
            Objects::String(s) => s.hash_value(),
            _ => 0,
        }
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
            Objects::HashMap(h) => h.obj_type(),
            Objects::HashKey(h) => h.obj_type(),
            Objects::HashPair(h) => h.obj_type(),
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
            Objects::HashMap(h) => h.inspect(),
            Objects::HashKey(h) => h.inspect(),
            Objects::HashPair(h) => h.inspect(),
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

    pub fn hash_value(&self) -> u64 {
        self.value as u64
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

    pub fn hash_value(&self) -> u64 {
        let mut s = DefaultHasher::new();
        self.hash(&mut s);
        s.finish()
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

    pub fn hash_value(&self) -> u64 {
        if self.value {
            1
        } else {
            0
        }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HashPair {
    pub key: Objects,
    pub value: Objects,
}

impl HashPair {
    pub fn new(key: Objects, value: Objects) -> Self {
        HashPair { key, value }
    }
}

impl Object for HashPair {
    fn obj_type(&self) -> ObjectType {
        HASH_OBJ
    }

    fn inspect(&self) -> String {
        format!("{}: {}", self.key.inspect(), self.value.inspect())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashKey {
    Integer(Integer),
    Boolean(Boolean),
    String(StringObject),
}

impl From<HashKey> for Objects {
    fn from(value: HashKey) -> Self {
        match value {
            HashKey::Integer(i) => Objects::Integer(i),
            HashKey::Boolean(b) => {
                let b_lit = native_bool_to_boolean_obj(b.value);

                Objects::Boolean(b_lit)
            }
            HashKey::String(s) => Objects::String(s),
        }
    }
}

impl HashKey {
    pub fn hash_key(&self) -> u64 {
        match self {
            HashKey::Integer(i) => i.hash_value(),
            HashKey::Boolean(b) => b.hash_value(),
            HashKey::String(s) => s.hash_value(),
        }
    }
}

impl Object for HashKey {
    fn obj_type(&self) -> ObjectType {
        match self {
            HashKey::Integer(i) => i.obj_type(),
            HashKey::Boolean(b) => b.obj_type(),
            HashKey::String(s) => s.obj_type(),
        }
    }

    fn inspect(&self) -> String {
        match self {
            HashKey::Integer(i) => i.inspect(),
            HashKey::Boolean(b) => b.inspect(),
            HashKey::String(s) => s.inspect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HashMapMonkey {
    pub pairs: HashMapMonkeyInner,
}

impl HashMapMonkey {
    pub fn new(pairs: HashMap<HashKey, HashPair>) -> Self {
        HashMapMonkey {
            pairs: HashMapMonkeyInner::new(pairs),
        }
    }
}

impl Object for HashMapMonkey {
    fn obj_type(&self) -> ObjectType {
        HASH_OBJ
    }

    fn inspect(&self) -> String {
        let mut s = String::new();
        s.push('{');
        let mut pairs: Vec<_> = self.pairs.0.iter().collect();
        for (i, (k, v)) in pairs.iter().enumerate() {
            s.push_str(&k.inspect());
            s.push_str(": ");
            s.push_str(&v.inspect());
            if i != pairs.len() - 1 {
                s.push_str(", ");
            }
        }
        s.push('}');
        s
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashMapMonkeyInner(pub HashMap<HashKey, HashPair>);

impl HashMapMonkeyInner {
    pub fn new(pairs: HashMap<HashKey, HashPair>) -> Self {
        HashMapMonkeyInner(pairs)
    }
}

impl Hash for HashMapMonkeyInner {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let keys: Vec<_> = self.0.keys().collect();
        for key in keys {
            key.hash(state);
            self.0.get(key).unwrap().hash(state);
        }
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

#[cfg(test)]
mod tests {
    use std::{
        collections::hash_map::DefaultHasher,
        hash::{Hash, Hasher},
    };

    use super::*;

    #[test]
    fn test_string_hash_key() {
        let hello1 = StringObject::new("Hello World");
        let hello2 = StringObject::new("Hello World");
        let diff1 = StringObject::new("My name is johnny");
        let diff2 = StringObject::new("My name is johnny");

        let mut hasher = DefaultHasher::new();
        hello1.hash(&mut hasher);
        let r1 = hasher.finish();

        let mut hasher = DefaultHasher::new();
        hello2.hash(&mut hasher);
        let r2 = hasher.finish();
        assert_eq!(r1, r2);

        let mut hasher = DefaultHasher::new();
        diff1.hash(&mut hasher);
        let r1 = hasher.finish();

        let mut hasher = DefaultHasher::new();
        diff2.hash(&mut hasher);
        let r2 = hasher.finish();
        assert_eq!(r1, r2);
    }
}
