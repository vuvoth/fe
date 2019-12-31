#[macro_use]
extern crate lazy_static;
extern crate regex;

pub mod ast;
pub mod errors;
pub mod parsers;
pub mod slice;
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

/// The location of a parser error.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Location {
    Span(Span),
    Eof,
}

/// An individual piece of information about a parse error.
#[derive(Debug, PartialEq)]
pub struct ParseErrorInfo {
    msg: String,
    loc: Location,
}

/// A collection of error messages describing an error that occurred during
/// parsing.
#[derive(Debug, PartialEq)]
pub struct ParseError(Vec<ParseErrorInfo>);

impl ParseError {
    pub fn new(msg: String, loc: Location) -> Self {
        Self(vec![ParseErrorInfo { msg, loc }])
    }

    pub fn at_eof(msg: String) -> Self {
        Self::new(msg, Location::Eof)
    }

    pub fn at_span(msg: String, span: Span) -> Self {
        Self::new(msg, Location::Span(span))
    }

    pub fn add_msg(mut self, msg: String, loc: Location) -> Self {
        self.0.push(ParseErrorInfo { msg, loc });
        self
    }
}

/// The primary container of information about the content being parsed during a
/// parsing operation.
pub struct ParseBuffer {
    /// The token list being parsed.
    tokens: Box<[Token]>,

    /// The original source code from which the token list was generated.
    source: String,
}

impl ParseBuffer {
    pub fn from_source(source: String) -> Result<Self, TokenizeError> {
        let tokens = tokenize(source.as_str())?;

        let tokens: Vec<_> = Vec::from(tokens)
            .into_iter()
            .filter(|t| match t.kind {
                TokenKind::Comment(_) => false,
                TokenKind::WhitespaceNewline => false,
                _ => true,
            })
            .collect();

        Ok(Self {
            tokens: tokens.into_boxed_slice(),
            source,
        })
    }

    #[inline]
    pub fn span_to_string(&self, span: Span) -> String {
        self.source[span.start..span.end].to_string()
    }

    fn begin(&self) -> Cursor {
        Cursor {
            ptr: &self.tokens[0],
            end: &self.tokens[self.tokens.len() - 1],
            buf: self,
        }
    }

    pub fn apply<P, O>(&self, parser: P) -> ParseResult<O>
    where
        P: Fn(Cursor) -> ParseResult<O>,
    {
        parser(self.begin())
    }
}

/// Indicates a parsing position in some parse buffer `buf`.
#[derive(Debug, Clone, Copy)]
pub struct Cursor {
    ptr: *const Token,
    end: *const Token,
    buf: *const ParseBuffer,
}

impl Cursor {
    #[inline]
    unsafe fn bump(self) -> Self {
        Cursor {
            ptr: self.ptr.offset(1),
            end: self.end,
            buf: self.buf,
        }
    }

    #[inline]
    pub fn eof(self) -> bool {
        self.ptr > self.end
    }

    #[inline]
    fn tok(self) -> Option<Token> {
        if self.eof() {
            None
        } else {
            Some(unsafe { *self.ptr })
        }
    }

    #[inline]
    pub unsafe fn span_to_string(&self, span: Span) -> String {
        (*self.buf).span_to_string(span)
    }

    #[inline]
    pub fn next_tok(&self) -> ParseResult<Token> {
        match self.tok() {
            None => Err(ParseError::at_eof(
                "reached end of parse buffer".to_string(),
            )),
            Some(tok) => Ok((unsafe { self.bump() }, tok)),
        }
    }
}

/// The result of a parsing function.
pub type ParseResult<O> = Result<(Cursor, O), ParseError>;

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
            let (_, tok) = input.next_tok()?;

            Err(err.add_msg(
                "many1: expected at least one occurrence".to_string(),
                Location::Span(tok.span),
            ))
        }
    }
}

pub fn many0<O, P>(parser: P) -> impl Fn(Cursor) -> ParseResult<Vec<O>>
where
    P: Fn(Cursor) -> ParseResult<O>,
{
    move |input| {
        if input.eof() {
            return Err(ParseError::at_eof("many0: can't parse at eof".to_string()));
        }

        let mut input = input;
        let mut results = vec![];

        loop {
            match parser(input) {
                Ok((next_input, next)) => {
                    input = next_input;
                    results.push(next);
                }
                Err(_) => break,
            }
        }

        Ok((input, results))
    }
}

pub fn preceded<O1, O2, F, G>(f: F, g: G) -> impl Fn(Cursor) -> ParseResult<O2>
where
    F: Fn(Cursor) -> ParseResult<O1>,
    G: Fn(Cursor) -> ParseResult<O2>,
{
    move |input| {
        let (input, _) = f(input)?;
        let (input, result) = g(input)?;
        Ok((input, result))
    }
}

pub fn terminated<O1, O2, F, G>(f: F, g: G) -> impl Fn(Cursor) -> ParseResult<O1>
where
    F: Fn(Cursor) -> ParseResult<O1>,
    G: Fn(Cursor) -> ParseResult<O2>,
{
    move |input| {
        let (input, result) = f(input)?;
        let (input, _) = g(input)?;
        Ok((input, result))
    }
}

pub fn pair<O1, O2, F, G>(f: F, g: G) -> impl Fn(Cursor) -> ParseResult<(O1, O2)>
where
    F: Fn(Cursor) -> ParseResult<O1>,
    G: Fn(Cursor) -> ParseResult<O2>,
{
    move |input| {
        let (input, result_1) = f(input)?;
        let (input, result_2) = g(input)?;
        Ok((input, (result_1, result_2)))
    }
}

pub fn separated_pair<O1, O2, O3, F, G, H>(
    f: F,
    g: G,
    h: H,
) -> impl Fn(Cursor) -> ParseResult<(O1, O3)>
where
    F: Fn(Cursor) -> ParseResult<O1>,
    G: Fn(Cursor) -> ParseResult<O2>,
    H: Fn(Cursor) -> ParseResult<O3>,
{
    move |input| {
        let (input, result_1) = f(input)?;
        let (input, _) = g(input)?;
        let (input, result_2) = h(input)?;
        Ok((input, (result_1, result_2)))
    }
}

pub fn opt<O, F>(f: F) -> impl Fn(Cursor) -> ParseResult<Option<O>>
where
    F: Fn(Cursor) -> ParseResult<O>,
{
    move |input| match f(input) {
        Ok((input_ok, result)) => Ok((input_ok, Some(result))),
        Err(_) => Ok((input, None)),
    }
}

pub fn map<O1, O2, P, F>(parser: P, mapper: F) -> impl Fn(Cursor) -> ParseResult<O2>
where
    P: Fn(Cursor) -> ParseResult<O1>,
    F: Fn(O1) -> O2,
{
    move |input| match parser(input) {
        Ok((input_ok, result)) => Ok((input_ok, mapper(result))),
        Err(err) => Err(err),
    }
}
