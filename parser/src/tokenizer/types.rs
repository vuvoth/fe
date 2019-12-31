use std::convert::TryFrom;
use std::str::Utf8Error;

use serde::{
    Deserialize,
    Serialize,
};

use crate::slice::{
    serde_str,
    Slice,
};
use crate::span::Span;
use crate::symbol::Symbol;

/// Indicates the basic syntactic element represented by a token.
#[derive(Serialize, Deserialize, Debug, PartialEq, Copy, Clone)]
pub enum TokenKind {
    #[serde(with = "serde_str")]
    Name(Slice<u8>),
    #[serde(with = "serde_str")]
    Num(Slice<u8>),
    #[serde(with = "serde_str")]
    Str(Slice<u8>),
    #[serde(with = "serde_str")]
    Comment(Slice<u8>),

    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Colon,
    Comma,
    Semi,
    Plus,
    Minus,
    Star,
    Slash,
    Pipe,
    Amper,
    Lt,
    Gt,
    Eq,
    Dot,
    Percent,
    OpenBrace,
    CloseBrace,
    EqEq,
    NotEq,
    LtEq,
    GtEq,
    Tilde,
    Caret,
    Shl,
    Shr,
    StarStar,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,
    AmperEq,
    PipeEq,
    CaretEq,
    ShlEq,
    ShrEq,
    StarStarEq,
    SlashSlash,
    SlashSlashEq,
    At,
    AtEq,
    RightArrow,
    Ellipsis,

    Indent,
    Dedent,

    // Grammatically significant newlines
    Newline,
    // Whitespace newlines (such as those that appear in parenthesized expressions)
    WhitespaceNewline,
    EndMarker,

    ErrorToken,
}

#[derive(Debug)]
pub enum MaybeToStrError {
    WrongVariant,
    Utf8Error(Utf8Error),
}

impl From<Utf8Error> for MaybeToStrError {
    fn from(err: Utf8Error) -> Self {
        Self::Utf8Error(err)
    }
}

impl TokenKind {
    pub fn maybe_to_str(&self) -> Result<&str, MaybeToStrError> {
        use TokenKind::*;

        let slice = match self {
            Name(s) => s,
            Num(s) => s,
            Str(s) => s,
            Comment(s) => s,
            _ => return Err(MaybeToStrError::WrongVariant),
        };

        Ok(unsafe { slice.as_str()? })
    }
}

impl TryFrom<&str> for TokenKind {
    type Error = &'static str;

    #[cfg_attr(tarpaulin, skip)]
    fn try_from(string: &str) -> Result<Self, Self::Error> {
        use TokenKind::*;

        Ok(match string {
            "(" => OpenParen,
            ")" => CloseParen,
            "[" => OpenBracket,
            "]" => CloseBracket,
            ":" => Colon,
            "," => Comma,
            ";" => Semi,
            "+" => Plus,
            "-" => Minus,
            "*" => Star,
            "/" => Slash,
            "|" => Pipe,
            "&" => Amper,
            "<" => Lt,
            ">" => Gt,
            "=" => Eq,
            "." => Dot,
            "%" => Percent,
            "{" => OpenBrace,
            "}" => CloseBrace,
            "==" => EqEq,
            "!=" => NotEq,
            "<=" => LtEq,
            ">=" => GtEq,
            "~" => Tilde,
            "^" => Caret,
            "<<" => Shl,
            ">>" => Shr,
            "**" => StarStar,
            "+=" => PlusEq,
            "-=" => MinusEq,
            "*=" => StarEq,
            "/=" => SlashEq,
            "%=" => PercentEq,
            "&=" => AmperEq,
            "|=" => PipeEq,
            "^=" => CaretEq,
            "<<=" => ShlEq,
            ">>=" => ShrEq,
            "**=" => StarStarEq,
            "//" => SlashSlash,
            "//=" => SlashSlashEq,
            "@" => At,
            "@=" => AtEq,
            "->" => RightArrow,
            "..." => Ellipsis,
            _ => return Err("unrecognized token string"),
        })
    }
}

/// A token parsed from a source string.
#[derive(Serialize, Deserialize, Debug, PartialEq, Copy, Clone)]
pub struct Token {
    /// The type of a token.
    pub kind: TokenKind,

    /// The span of source text covered by a token.
    pub span: Span,
}

impl Token {
    pub fn maybe_to_symbol(&self) -> Result<Symbol, MaybeToStrError> {
        self.kind.maybe_to_str().map(Symbol::new)
    }
}

impl From<&Token> for Span {
    fn from(token: &Token) -> Span {
        token.span
    }
}
