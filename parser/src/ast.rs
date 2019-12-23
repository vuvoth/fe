use std::convert::TryFrom;

use serde::{
    Deserialize,
    Serialize,
};

use crate::span::Spanned;
use crate::tokenizer::types::Token;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Module {
    pub body: Vec<Spanned<ModuleStmt>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ModuleStmt {
    ContractDef {
        name: String,
        body: Vec<Spanned<ContractStmt>>,
    },
    SimpleImport {
        names: Vec<Spanned<SimpleImportName>>,
    },
    FromImport {
        path: Spanned<FromImportPath>,
        names: Spanned<FromImportNames>,
    },
    TypeDef {
        name: String,
        typ: TypeDesc,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ContractStmt {
    EventDef {
        name: String,
        fields: Vec<Spanned<EventField>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct SimpleImportName {
    pub path: Vec<String>,
    pub alias: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum FromImportPath {
    Absolute {
        path: Vec<String>,
    },
    Relative {
        parent_level: usize,
        path: Vec<String>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum FromImportNames {
    Star,
    List(Vec<Spanned<FromImportName>>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct FromImportName {
    pub name: String,
    pub alias: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum TypeDesc {
    Base {
        base: String,
    },
    Array {
        typ: Box<Spanned<TypeDesc>>,
        dimension: usize,
    },
    Map {
        from: Box<Spanned<TypeDesc>>,
        to: Box<Spanned<TypeDesc>>,
    },
}

impl<'a> From<&'a Token<'a>> for Spanned<TypeDesc> {
    fn from(token: &'a Token<'a>) -> Self {
        Spanned {
            node: TypeDesc::Base {
                base: token.string.to_string(),
            },
            span: token.span,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct EventField {
    pub name: String,
    pub typ: Spanned<TypeDesc>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Pow,
    LShift,
    RShift,
    BitOr,
    BitXor,
    BitAnd,
}

impl TryFrom<&str> for Operator {
    type Error = &'static str;

    #[cfg_attr(tarpaulin, skip)]
    fn try_from(string: &str) -> Result<Self, Self::Error> {
        match string {
            "+" => Ok(Self::Add),
            "-" => Ok(Self::Sub),
            "*" => Ok(Self::Mult),
            "/" => Ok(Self::Div),
            "%" => Ok(Self::Mod),
            "**" => Ok(Self::Pow),
            "<<" => Ok(Self::LShift),
            ">>" => Ok(Self::RShift),
            "|" => Ok(Self::BitOr),
            "^" => Ok(Self::BitXor),
            "&" => Ok(Self::BitAnd),
            _ => Err("unrecognized binary operator string"),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum UnaryOp {
    Invert,
    Not,
    UAdd,
    USub,
}

impl TryFrom<&str> for UnaryOp {
    type Error = &'static str;

    #[cfg_attr(tarpaulin, skip)]
    fn try_from(string: &str) -> Result<Self, Self::Error> {
        match string {
            "~" => Ok(Self::Invert),
            "not" => Ok(Self::Not),
            "+" => Ok(Self::UAdd),
            "-" => Ok(Self::USub),
            _ => Err("unrecognized unary operator string"),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ConstExpr {
    BinOp {
        left: Box<Spanned<ConstExpr>>,
        op: Operator,
        right: Box<Spanned<ConstExpr>>,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<Spanned<ConstExpr>>,
    },
    Name {
        name: String,
    },
    Num {
        num: String,
    },
}
