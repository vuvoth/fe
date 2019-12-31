//! Heavily inspired by (some might say ported from) [Python's std lib `tokenize` module](https://github.com/python/cpython/blob/2a58b0636d1f620f8a85a2e4c030cc10551936a5/Lib/tokenize.py).
//!
//! ## Usage
//!
//! ```rust
//! use vyper_parser::tokenizer::tokenize;
//!
//! let source_string = r#"
//! class Foo:
//!     bar = "baz"
//! "#;
//! let token_vector = tokenize(source_string).unwrap();
//! ```
//!
//! ## Differences/similarities with python's `tokenize` module
//!
//! * The [`self::tokenize::tokenize`] function generates all token types (which
//!   we call token kinds) produced by python's `tokenize.tokenize` method
//!   *except* for an initial `ENCODING` token.  Source files are always assumed
//!   to be encoded in utf-8 so this token is redundant.
//! * Unlike the [Python std lib `tokenize`
//!   module](https://github.com/python/cpython/blob/
//!   2a58b0636d1f620f8a85a2e4c030cc10551936a5/Lib/tokenize.py), our tokenizer
//!   uses separate tokens for different operator tokens, instead of using a
//!   single op token to represent all operator tokens.
//! * The [`self::types::Token`] struct, which we use to represent parsed
//!   tokens, in most cases only includes span information to indicate the
//!   string content of a token.  Token consumers are therefore generally
//!   expected to have access to the source string from which tokens were
//!   generated if they wish to obtain the string content of tokens. The only
//!   token type that independently includes a means to obtain string content
//!   (name tokens) includes indices into a string interner table (called
//!   symbols) which can also be used to obtain string content.
//! * The [`self::tokenize::tokenize`] function has no streaming behavior and
//!   accepts a reference to an entire source string available in memory.
//!   Python's `tokenize.tokenize` function accepts a reference to a function
//!   that progressively yields lines of text from a source file.
//!
//! As Python's `tokenize` module's implementation is pretty ugly, so is the
//! implementation of [`self::tokenize::tokenize`].  It may be a candidate for
//! later cleanup.

mod regex;
pub mod tokenize;
pub mod types;
pub mod wasm;

pub use self::tokenize::{
    tokenize,
    TokenizeError,
};
pub use self::types::{
    Token,
    TokenKind,
};
