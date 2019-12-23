use serde::{
    Deserialize,
    Serialize,
};

use crate::span::Span;

/// Indicates the basic syntactic element represented by a token.
#[derive(Serialize, Deserialize, Debug, PartialEq, Copy, Clone)]
pub enum TokenKind {
    Name,
    Num,
    Str,
    Op,
    Comment,

    Indent,
    Dedent,

    // Grammatically significant newlines
    Newline,
    // Whitespace newlines (such as those that appear in parenthesized expressions)
    WhitespaceNewline,
    EndMarker,

    ErrorToken,
}

/// A token parsed from a source string.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Token<'a> {
    /// The type of a token.
    pub kind: TokenKind,

    /// The text content of a parsed token.
    pub string: &'a str,

    /// The span of source text covered by a token.
    pub span: Span,

    /// The text content of the line from which a token was parsed.
    pub line: &'a str,
}

impl<'a> From<&Token<'a>> for Span {
    fn from(token: &Token) -> Span {
        token.span
    }
}
