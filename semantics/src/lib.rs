//! Fe semantic analysis.
//!
//! This library is used to analyze the semantics of a given Fe AST. It detects
//! any semantic errors within a given AST and produces a `Context` instance
//! that can be used to query contextual information attributed to AST nodes.

pub mod errors;
pub mod namespace;
mod traversal;

use crate::errors::SemanticError;
use crate::namespace::events::Event;
use crate::namespace::scopes::Shared;
use crate::namespace::types::{
    FixedSize,
    Type,
};
use fe_parser::ast as fe;
use fe_parser::span::{
    Span,
    Spanned,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Indicates where an expression is stored.
#[derive(Clone, Debug, PartialEq)]
pub enum Location {
    Storage { index: usize },
    Memory,
    Value,
}

/// Contains contextual information relating to an expression AST node.
#[derive(Clone, Debug, PartialEq)]
pub struct ExpressionAttributes {
    pub typ: Type,
    pub location: Location,
}

/// Contains contextual information relating to a function definition AST node.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionAttributes {
    pub name: String,
    pub param_types: Vec<FixedSize>,
    pub return_type: Option<FixedSize>,
}

/// Contains contextual information about a Fe module and can be queried using
/// `Spanned` AST nodes.
#[derive(Clone, Debug, Default)]
pub struct Context {
    expressions: HashMap<Span, ExpressionAttributes>,
    emits: HashMap<Span, Event>,
    functions: HashMap<Span, FunctionAttributes>,
    declarations: HashMap<Span, FixedSize>,
    // We will want to attribute more information to contracts. Things like events, public fields,
    // public functions, and runtime functions will all be needed during mapping. These things
    // should be grouped together under a new `ContractAttributes` struct.
    contracts: HashMap<Span, Vec<FunctionAttributes>>,
}

impl Context {
    pub fn new_shared() -> Shared<Self> {
        Rc::new(RefCell::new(Self::new()))
    }

    pub fn new() -> Self {
        Context {
            expressions: HashMap::new(),
            emits: HashMap::new(),
            functions: HashMap::new(),
            declarations: HashMap::new(),
            contracts: HashMap::new(),
        }
    }

    /// Attribute contextual information to an expression node.
    pub fn add_expression(
        &mut self,
        spanned: &Spanned<fe::Expr>,
        attributes: ExpressionAttributes,
    ) {
        self.expressions.insert(spanned.span, attributes);
    }

    /// Get information that has been attributed to an expression node.
    pub fn get_expression(&self, spanned: &Spanned<fe::Expr>) -> Option<&ExpressionAttributes> {
        self.expressions.get(&spanned.span)
    }

    /// Attribute contextual information to an emit statement node.
    pub fn add_emit(&mut self, spanned: &Spanned<fe::FuncStmt>, event: Event) {
        self.emits.insert(spanned.span, event);
    }

    /// Get information that has been attributed to an emit statement node.
    pub fn get_emit(&self, spanned: &Spanned<fe::FuncStmt>) -> Option<&Event> {
        self.emits.get(&spanned.span)
    }

    /// Attribute contextual information to a function definition node.
    pub fn add_function(
        &mut self,
        spanned: &Spanned<fe::ContractStmt>,
        attributes: FunctionAttributes,
    ) {
        self.functions.insert(spanned.span, attributes);
    }

    /// Get information that has been attributed to a function definition node.
    pub fn get_function(&self, spanned: &Spanned<fe::ContractStmt>) -> Option<&FunctionAttributes> {
        self.functions.get(&spanned.span)
    }

    /// Attribute contextual information to a declaration node.
    pub fn add_declaration(&mut self, spanned: &Spanned<fe::FuncStmt>, typ: FixedSize) {
        self.declarations.insert(spanned.span, typ);
    }

    /// Get information that has been attributed to a declaration node.
    pub fn get_declaration(&self, spanned: &Spanned<fe::FuncStmt>) -> Option<&FixedSize> {
        self.declarations.get(&spanned.span)
    }

    /// Attribute contextual information to a contract definition node.
    pub fn add_contract(
        &mut self,
        spanned: &Spanned<fe::ModuleStmt>,
        functions: Vec<FunctionAttributes>,
    ) {
        self.contracts.insert(spanned.span, functions);
    }

    /// Get information that has been attributed to a contract definition node.
    pub fn get_contract(
        &self,
        spanned: &Spanned<fe::ModuleStmt>,
    ) -> Option<&Vec<FunctionAttributes>> {
        self.contracts.get(&spanned.span)
    }
}

/// Performs semantic analysis of the source program and returns a `Context`
/// instance.
pub fn analysis(module: &fe::Module) -> Result<Context, SemanticError> {
    let context = Context::new_shared();
    traversal::module::module(Rc::clone(&context), module)?;
    Ok(Rc::try_unwrap(context)
        .map_err(|_| "more than one strong reference pointing to context")
        // This should never panic.
        .expect("failed to unwrap reference counter")
        .into_inner())
}

pub mod test_utils {
    use crate::namespace::types::FixedSize;
    use crate::{
        Context,
        ExpressionAttributes,
    };
    use fe_parser::ast as fe;
    use fe_parser::span::{
        Span,
        Spanned,
    };

    pub struct ContextHarness {
        pub context: Context,
        pub src: String,
    }

    impl ContextHarness {
        pub fn new(src: &str) -> Self {
            ContextHarness {
                context: Context::new(),
                src: src.to_string(),
            }
        }

        fn find_span(&self, substr: &str) -> Span {
            let start = self
                .src
                .find(substr)
                .unwrap_or_else(|| panic!("unable to find '{}' in '{}'", substr, self.src));

            Span {
                start,
                end: start + substr.len(),
            }
        }

        pub fn add_expression(&mut self, substr: &str, attributes: ExpressionAttributes) {
            let span = self.find_span(substr);
            let mock_spanned = Spanned {
                span,
                node: fe::Expr::Name("foo"),
            };
            self.context.add_expression(&mock_spanned, attributes)
        }

        pub fn add_declaration(&mut self, substr: &str, typ: FixedSize) {
            let span = self.find_span(substr);
            let mock_spanned = Spanned {
                span,
                node: fe::FuncStmt::Expr {
                    value: fe::Expr::Name("foo"),
                },
            };
            self.context.add_declaration(&mock_spanned, typ)
        }
    }
}
