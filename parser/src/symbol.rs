use std::fmt;
use std::sync::Mutex;

use serde::de::{
    self,
    Visitor,
};
use serde::{
    Deserialize,
    Deserializer,
    Serialize,
    Serializer,
};
use string_interner::Sym;
use string_interner::{
    DefaultStringInterner,
    StringInterner,
};

lazy_static! {
    static ref INTERNER: Mutex<DefaultStringInterner> = Mutex::new(StringInterner::default());
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Symbol(Sym);

impl Symbol {
    pub fn new(string: &str) -> Self {
        Self(INTERNER.lock().unwrap().get_or_intern(string))
    }

    pub fn as_str(&self) -> &'static str {
        let interner = INTERNER.lock().unwrap();
        let interned_slice = interner.resolve(self.0).unwrap();

        unsafe { std::mem::transmute::<&str, &str>(interned_slice) }
    }
}

impl Serialize for Symbol {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl<'de> Deserialize<'de> for Symbol {
    fn deserialize<D>(deserializer: D) -> Result<Symbol, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(SymbolVisitor)
    }
}

struct SymbolVisitor;

impl<'de> Visitor<'de> for SymbolVisitor {
    type Value = Symbol;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a string")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(Symbol::new(value))
    }
}
