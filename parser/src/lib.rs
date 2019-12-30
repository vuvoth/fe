#[macro_use]
extern crate lazy_static;
extern crate regex;

pub mod ast;
pub mod errors;
pub mod parsers;
pub mod span;
pub mod string_utils;
pub mod symbol;
pub mod tokenizer;

use span::Span;
use tokenizer::{
    tokenize,
    Token,
    TokenKind,
    TokenizeError,
};

#[derive(Debug, PartialEq)]
pub enum Location {
    Span(Span),
    Eof,
}

#[derive(Debug, PartialEq)]
pub struct ParseErrorInfo {
    msg: String,
    loc: Location,
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    One(ParseErrorInfo),
    Many(Vec<ParseErrorInfo>),
}

impl ParseError {
    pub fn new(msg: String, loc: Location) -> Self {
        Self::One(ParseErrorInfo { msg, loc })
    }

    pub fn at_eof(msg: String) -> Self {
        Self::new(msg, Location::Eof)
    }

    pub fn at_span(msg: String, span: Span) -> Self {
        Self::new(msg, Location::Span(span))
    }

    pub fn add_msg(self, msg: String, loc: Location) -> Self {
        let mut infos: Vec<ParseErrorInfo> = vec![];

        match self {
            Self::One(info) => infos.push(info),
            Self::Many(mut other_infos) => infos.append(&mut other_infos),
        }

        infos.push(ParseErrorInfo { msg, loc });

        Self::Many(infos)
    }
}

pub struct ParseBuffer {
    /// The token buffer being parsed.
    tokens: Box<[Token]>,

    /// The original source code from which the token buffer was generated.
    source: String,
}

#[derive(Debug, Clone, Copy)]
pub struct Cursor {
    ptr: *const Token,
    end: *const Token,
    buf: *const ParseBuffer,
}

pub type ParseResult<O> = Result<(Cursor, O), ParseError>;

impl ParseBuffer {
    pub fn from_source(source: String) -> Result<Self, TokenizeError> {
        let tokens = tokenize(source.as_str())?;

        let tokens: Vec<_> = tokens
            .into_iter()
            .filter(|t| match t.kind {
                TokenKind::Comment => false,
                TokenKind::WhitespaceNewline => false,
                _ => true,
            })
            .collect();

        Ok(Self {
            tokens: tokens.into_boxed_slice(),
            source,
        })
    }

    pub fn span_to_string(&self, span: Span) -> String {
        self.source[span.start..span.end].to_string()
    }

    fn begin(&self) -> Cursor {
        Cursor::new(&self.tokens[0], &self.tokens[self.tokens.len() - 1], self)
    }

    pub fn apply<P, O>(&self, parser: P) -> ParseResult<O>
    where
        P: Fn(Cursor) -> ParseResult<O>,
    {
        parser(self.begin())
    }
}

impl Cursor {
    #[inline]
    pub fn new(ptr: *const Token, end: *const Token, buf: *const ParseBuffer) -> Self {
        Cursor { ptr, end, buf }
    }

    #[inline]
    fn bump(self) -> Self {
        unsafe { Self::new(self.ptr.offset(1), self.end, self.buf) }
    }

    #[inline]
    pub fn eof(self) -> bool {
        self.ptr > self.end
    }

    #[inline]
    fn token(self) -> Option<Token> {
        if self.eof() {
            None
        } else {
            Some(unsafe { *self.ptr })
        }
    }

    #[inline]
    fn next(&self) -> ParseResult<Token> {
        match self.token() {
            None => Err(ParseError::at_eof(
                "reached end of parse buffer".to_string(),
            )),
            Some(tok) => Ok((self.bump(), tok)),
        }
    }
}

macro_rules! succ {
  (0, $macro:ident ! ($($args:tt)*)) => ($macro!(1, $($args)*));
  (1, $macro:ident ! ($($args:tt)*)) => ($macro!(2, $($args)*));
  (2, $macro:ident ! ($($args:tt)*)) => ($macro!(3, $($args)*));
  (3, $macro:ident ! ($($args:tt)*)) => ($macro!(4, $($args)*));
  (4, $macro:ident ! ($($args:tt)*)) => ($macro!(5, $($args)*));
  (5, $macro:ident ! ($($args:tt)*)) => ($macro!(6, $($args)*));
  (6, $macro:ident ! ($($args:tt)*)) => ($macro!(7, $($args)*));
}

pub trait Alt<O> {
    fn parse(&self, input: Cursor) -> ParseResult<O>;
}

macro_rules! alt_trait_impl {
    ($($type_var:ident)+) => {
        impl<O, $($type_var),+> Alt<O> for ($($type_var),+)
        where
            $($type_var: Fn(Cursor) -> ParseResult<O>),+
        {
            fn parse(&self, input: Cursor) -> ParseResult<O> {
                alt_parse_body!(0, self, input, pos, $($type_var)+)
            }
        }
    };
}

macro_rules! alt_parse_body {
    ($id:tt, $self:expr, $input:expr, $pos:expr, $head:ident $($tail:ident)+) => {{
        let result = $self.$id($input);
        if result.is_ok() {
            return result;
        }
        succ!($id, alt_parse_body!($self, $input, $pos, $($tail)+))
    }};
    ($id:tt, $self:expr, $input:expr, $pos:expr, $head:ident) => {{
        $self.$id($input)
    }};
}

alt_trait_impl!(A B);
alt_trait_impl!(A B C);
alt_trait_impl!(A B C D);
alt_trait_impl!(A B C D E);
alt_trait_impl!(A B C D E F);
alt_trait_impl!(A B C D E F G);
alt_trait_impl!(A B C D E F G H);

pub fn alt<O, A: Alt<O>>(alts: A) -> impl Fn(Cursor) -> ParseResult<O> {
    move |input| alts.parse(input)
}

pub fn many1<O, P>(parser: P) -> impl Fn(Cursor) -> ParseResult<Vec<O>>
where
    P: Fn(Cursor) -> ParseResult<O>,
{
    move |input| match parser(input) {
        Ok((input, first)) => {
            let mut input = input;
            let mut results = vec![first];

            loop {
                match parser(input) {
                    Ok((input_, next)) => {
                        input = input_;
                        results.push(next);
                    }
                    Err(_) => break,
                }
            }

            Ok((input, results))
        }
        Err(err) => {
            let (_, tok) = input.next()?;

            Err(err.add_msg(
                "many1: expected at least one occurrence".to_string(),
                Location::Span(tok.span),
            ))
        }
    }
}

pub fn terminated<O1, O2, F, G>(f: F, g: G) -> impl Fn(Cursor) -> ParseResult<O1>
where
    F: Fn(Cursor) -> ParseResult<O1>,
    G: Fn(Cursor) -> ParseResult<O2>,
{
    move |input| {
        let (input, content) = f(input)?;
        let (input, _) = g(input)?;
        Ok((input, content))
    }
}
