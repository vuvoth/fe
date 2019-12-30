use std::convert::TryFrom;

use serde::{
    Deserialize,
    Serialize,
};

use crate::span::Span;
use crate::symbol::Symbol;

/// Indicates the basic syntactic element represented by a token.
#[derive(Serialize, Deserialize, Debug, PartialEq, Copy, Clone)]
pub enum TokenKind {
    Name(Symbol),
    Num,
    Str,
    Comment,

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

impl TokenKind {
    pub fn maybe_to_symbol(&self) -> Option<Symbol> {
        match self {
            Self::Name(s) => Some(*s),
            _ => return None,
        }
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
    pub fn maybe_to_symbol(&self) -> Option<Symbol> {
        self.kind.maybe_to_symbol()
    }
}

impl From<&Token> for Span {
    fn from(token: &Token) -> Span {
        token.span
    }
}
