use std::convert::TryFrom;

use serde::{
    Deserialize,
    Serialize,
};

use crate::span::Spanned;
use crate::symbol::Symbol;
use crate::tokenizer::types::{
    Token,
    TokenKind,
};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Module {
    pub body: Vec<Spanned<ModuleStmt>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ModuleStmt {
    ContractDef {
        name: Symbol,
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
        name: Symbol,
        typ: TypeDesc,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ContractStmt {
    EventDef {
        name: Symbol,
        fields: Vec<Spanned<EventField>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct SimpleImportName {
    pub path: Vec<Symbol>,
    pub alias: Option<Symbol>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum FromImportPath {
    Absolute {
        path: Vec<Symbol>,
    },
    Relative {
        parent_level: usize,
        path: Vec<Symbol>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum FromImportNames {
    Star,
    List(Vec<Spanned<FromImportName>>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct FromImportName {
    pub name: Symbol,
    pub alias: Option<Symbol>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum TypeDesc {
    Base {
        base: Symbol,
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

impl From<&Token> for Spanned<TypeDesc> {
    fn from(token: &Token) -> Self {
        Spanned {
            node: TypeDesc::Base {
                base: token.maybe_to_symbol().unwrap(),
            },
            span: token.span,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct EventField {
    pub name: Symbol,
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

impl TryFrom<&Token> for Operator {
    type Error = &'static str;

    #[cfg_attr(tarpaulin, skip)]
    fn try_from(tok: &Token) -> Result<Self, Self::Error> {
        use TokenKind::*;

        match tok.kind {
            Plus => Ok(Self::Add),
            Minus => Ok(Self::Sub),
            Star => Ok(Self::Mult),
            Slash => Ok(Self::Div),
            Percent => Ok(Self::Mod),
            StarStar => Ok(Self::Pow),
            Shl => Ok(Self::LShift),
            Shr => Ok(Self::RShift),
            Pipe => Ok(Self::BitOr),
            Caret => Ok(Self::BitXor),
            Amper => Ok(Self::BitAnd),
            _ => Err("unrecognized binary operator token"),
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

impl TryFrom<&Token> for UnaryOp {
    type Error = &'static str;

    #[cfg_attr(tarpaulin, skip)]
    fn try_from(tok: &Token) -> Result<Self, Self::Error> {
        use TokenKind::*;

        match tok.kind {
            Tilde => Ok(Self::Invert),
            Name(s) if s == Symbol::new("not") => Ok(Self::Not),
            Plus => Ok(Self::UAdd),
            Minus => Ok(Self::USub),
            _ => Err("unrecognized unary operator token"),
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
        name: Symbol,
    },
    Num {
        num: String,
    },
}
