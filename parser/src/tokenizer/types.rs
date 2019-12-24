use std::convert::TryFrom;

use serde::{
    Deserialize,
    Serialize,
};

use crate::span::Span;

/// Indicates the basic syntactic element represented by a token.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum TokenKind {
    Name(String),
    Num(String),
    Str(String),
    Comment(String),

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
    pub fn maybe_to_string(&self) -> Option<String> {
        use TokenKind::*;

        Some(match self {
            Name(s) => s.clone(),
            Num(s) => s.clone(),
            Str(s) => s.clone(),
            Comment(s) => s.clone(),
            OpenParen => "(".to_string(),
            CloseParen => ")".to_string(),
            OpenBracket => "[".to_string(),
            CloseBracket => "]".to_string(),
            Colon => ":".to_string(),
            Comma => ",".to_string(),
            Semi => ";".to_string(),
            Plus => "+".to_string(),
            Minus => "-".to_string(),
            Star => "*".to_string(),
            Slash => "/".to_string(),
            Pipe => "|".to_string(),
            Amper => "&".to_string(),
            Lt => "<".to_string(),
            Gt => ">".to_string(),
            Eq => "=".to_string(),
            Dot => ".".to_string(),
            Percent => "%".to_string(),
            OpenBrace => "{".to_string(),
            CloseBrace => "}".to_string(),
            EqEq => "==".to_string(),
            NotEq => "!=".to_string(),
            LtEq => "<=".to_string(),
            GtEq => ">=".to_string(),
            Tilde => "~".to_string(),
            Caret => "^".to_string(),
            Shl => "<<".to_string(),
            Shr => ">>".to_string(),
            StarStar => "**".to_string(),
            PlusEq => "+=".to_string(),
            MinusEq => "-=".to_string(),
            StarEq => "*=".to_string(),
            SlashEq => "/=".to_string(),
            PercentEq => "%=".to_string(),
            AmperEq => "&=".to_string(),
            PipeEq => "|=".to_string(),
            CaretEq => "^=".to_string(),
            ShlEq => "<<=".to_string(),
            ShrEq => ">>=".to_string(),
            StarStarEq => "**=".to_string(),
            SlashSlash => "//".to_string(),
            SlashSlashEq => "//=".to_string(),
            At => "@".to_string(),
            AtEq => "@=".to_string(),
            RightArrow => "->".to_string(),
            Ellipsis => "...".to_string(),
            Newline => "\n".to_string(),
            WhitespaceNewline => "\n".to_string(),
            _ => return None,
        })
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
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Token {
    /// The type of a token.
    pub kind: TokenKind,

    /// The span of source text covered by a token.
    pub span: Span,
}

impl Token {
    pub fn maybe_to_string(&self) -> Option<String> {
        self.kind.maybe_to_string()
    }
}

impl From<&Token> for Span {
    fn from(token: &Token) -> Span {
        token.span
    }
}
