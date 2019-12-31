use std::convert::TryFrom;

use crate::ast::ModuleStmt::*;
use crate::ast::*;
use crate::span::{
    Span,
    Spanned,
};
use crate::symbol::Symbol;
use crate::tokenizer::types::{
    Token,
    TokenKind,
    TokenKind::*,
};
use crate::{
    alt,
    many0,
    many1,
    map,
    opt,
    pair,
    preceded,
    separated_pair,
    terminated,
    Cursor,
    ParseError,
    ParseResult,
};

/// Parse a token of a specific kind.
pub fn token(kind: TokenKind) -> impl Fn(Cursor) -> ParseResult<Token> {
    move |input| {
        let (input, tok) = input.next_tok()?;

        if tok.kind == kind {
            Ok((input, tok))
        } else {
            Err(ParseError::at_span(
                format!("expected {:?}, found {:?}", kind, tok.kind),
                tok.span,
            ))
        }
    }
}

/// Parse a name token.
pub fn name_token(input: Cursor) -> ParseResult<Token> {
    let (input, tok) = input.next_tok()?;

    match tok.kind {
        Name(_) => Ok((input, tok)),
        _ => Err(ParseError::at_span(
            format!("expected name token, found {:?}", tok.kind),
            tok.span,
        )),
    }
}

/// Parse a num token.
pub fn num_token(input: Cursor) -> ParseResult<Token> {
    let (input, tok) = input.next_tok()?;

    match tok.kind {
        Num(_) => Ok((input, tok)),
        _ => Err(ParseError::at_span(
            format!("expected name token, found {:?}", tok.kind),
            tok.span,
        )),
    }
}

/// Parse a name token that contains a specific symbol.
pub fn name_string<'a>(string: &'a str) -> impl Fn(Cursor) -> ParseResult<Token> + 'a {
    move |input| {
        let (input, tok) = input.next_tok()?;

        match tok.kind {
            Name(s) if unsafe { s.as_str().unwrap() } == string => Ok((input, tok)),
            _ => Err(ParseError::at_span(
                format!("expected name \"{}\", found {:?}", string, tok.kind),
                tok.span,
            )),
        }
    }
}

/// Parse a module definition.
pub fn file_input(input: Cursor) -> ParseResult<Spanned<Module>> {
    alt((empty_file_input, non_empty_file_input))(input)
}

/// Parse an empty module definition.
pub fn empty_file_input(input: Cursor) -> ParseResult<Spanned<Module>> {
    // ENDMARKER
    let (input, end_tok) = token(EndMarker)(input)?;

    Ok((
        input,
        Spanned {
            node: Module { body: vec![] },
            span: end_tok.span,
        },
    ))
}

/// Parse a non-empty module definition.
pub fn non_empty_file_input(input: Cursor) -> ParseResult<Spanned<Module>> {
    // module_stmt+
    let (input, body) = many1(module_stmt)(input)?;

    // ENDMARKER
    let (input, _) = token(EndMarker)(input)?;

    let span = {
        let first = body.first().unwrap();
        let last = body.last().unwrap();

        Span::from_pair(first, last)
    };

    Ok((
        input,
        Spanned {
            node: Module { body },
            span,
        },
    ))
}

/// Parse a module statement, such as a contract definition.
pub fn module_stmt(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    alt((import_stmt, contract_def))(input)
}

/// Parse an import statement.
pub fn import_stmt(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    terminated(alt((simple_import, from_import)), token(Newline))(input)
}

/// Parse an import statement beginning with the "import" keyword.
pub fn simple_import(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    let (input, import_kw) = name_string("import")(input)?;
    let (input, first_name) = simple_import_name(input)?;
    let (input, mut other_names) = many0(preceded(token(Comma), simple_import_name))(input)?;

    let mut result = vec![first_name];
    result.append(&mut other_names);

    let span = {
        let last = result.last().unwrap();
        Span::from_pair(&import_kw, last)
    };

    Ok((
        input,
        Spanned {
            node: SimpleImport { names: result },
            span,
        },
    ))
}

/// Parse a name that is being imported by a simple import statement.
pub fn simple_import_name(input: Cursor) -> ParseResult<Spanned<SimpleImportName>> {
    let (input, path) = dotted_name(input)?;
    let (input, alias_tok) = opt(preceded(name_string("as"), name_token))(input)?;

    let (span, alias) = {
        match alias_tok {
            Some(tok) => (
                Span::from_pair(&path, &tok),
                Some(tok.maybe_to_symbol().unwrap()),
            ),
            _ => (path.span, None),
        }
    };

    Ok((
        input,
        Spanned {
            node: SimpleImportName {
                path: path.node,
                alias,
            },
            span,
        },
    ))
}

/// Parse an import statement beginning with the "from" keyword.
pub fn from_import(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    alt((from_import_parent_alt, from_import_sub_alt))(input)
}

/// Parse a "from" import with a path that contains only parent module
/// components.
pub fn from_import_parent_alt(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    let (input, from_kw) = name_string("from")(input)?;
    let (input, parent_level) = dots_to_int(input)?;
    let (input, _) = name_string("import")(input)?;
    let (input, names) = from_import_names(input)?;

    let path = Spanned {
        node: FromImportPath::Relative {
            parent_level: parent_level.node,
            path: vec![],
        },
        span: parent_level.span,
    };
    let span = Span::from_pair(&from_kw, names.span);

    Ok((
        input,
        Spanned {
            node: FromImport { path, names },
            span,
        },
    ))
}

/// Parse a "from" import with a path that contains sub module components.
pub fn from_import_sub_alt(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    let (input, from_kw) = name_string("from")(input)?;
    let (input, path) = from_import_sub_path(input)?;
    let (input, _) = name_string("import")(input)?;
    let (input, names) = from_import_names(input)?;

    let span = Span::from_pair(&from_kw, names.span);

    Ok((
        input,
        Spanned {
            node: FromImport { path, names },
            span,
        },
    ))
}

/// Parse a path containing sub module components in a "from" import statement.
pub fn from_import_sub_path(input: Cursor) -> ParseResult<Spanned<FromImportPath>> {
    let (input, opt_parent_level) = opt(dots_to_int)(input)?;
    let (input, dotted_name) = dotted_name(input)?;

    let result = match opt_parent_level {
        Some(parent_level) => {
            let span = Span::from_pair(&parent_level, &dotted_name);
            Spanned {
                node: FromImportPath::Relative {
                    parent_level: parent_level.node,
                    path: dotted_name.node,
                },
                span,
            }
        }
        None => Spanned {
            node: FromImportPath::Absolute {
                path: dotted_name.node,
            },
            span: dotted_name.span,
        },
    };

    Ok((input, result))
}

/// Parse the names to be imported by a "from" import statement.
pub fn from_import_names(input: Cursor) -> ParseResult<Spanned<FromImportNames>> {
    alt((
        from_import_names_star,
        from_import_names_parens,
        from_import_names_list,
    ))(input)
}

/// Parse a wildcard token ("*") in a "from" import statement.
pub fn from_import_names_star(input: Cursor) -> ParseResult<Spanned<FromImportNames>> {
    let (input, star) = token(Star)(input)?;

    Ok((
        input,
        Spanned {
            node: FromImportNames::Star,
            span: star.span,
        },
    ))
}

/// Parse a parenthesized list of names to be imported by a "from" import
/// statement.
pub fn from_import_names_parens(input: Cursor) -> ParseResult<Spanned<FromImportNames>> {
    let (input, o_paren) = token(OpenParen)(input)?;
    let (input, names) = from_import_names_list(input)?;
    let (input, c_paren) = token(CloseParen)(input)?;

    Ok((
        input,
        Spanned {
            node: names.node,
            span: Span::from_pair(&o_paren, &c_paren),
        },
    ))
}

/// Parse a list of names to be imported by a "from" import statement.
pub fn from_import_names_list(input: Cursor) -> ParseResult<Spanned<FromImportNames>> {
    let (input, first_name) = from_import_name(input)?;
    let (input, mut other_names) = many0(preceded(token(Comma), from_import_name))(input)?;
    let (input, comma_tok) = opt(token(Comma))(input)?;

    let mut names = vec![first_name];
    names.append(&mut other_names);

    let span = {
        let first = names.first().unwrap();
        match comma_tok {
            Some(tok) => Span::from_pair(first, &tok),
            None => {
                let last = names.last().unwrap();
                Span::from_pair(first, last)
            }
        }
    };

    Ok((
        input,
        Spanned {
            node: FromImportNames::List(names),
            span,
        },
    ))
}

/// Parse an import name with an optional alias in a "from" import statement.
pub fn from_import_name(input: Cursor) -> ParseResult<Spanned<FromImportName>> {
    let (input, name_tok) = name_token(input)?;
    let (input, alias_tok) = opt(preceded(name_string("as"), name_token))(input)?;

    let name = name_tok.maybe_to_symbol().unwrap();
    let (span, alias) = {
        match alias_tok {
            Some(tok) => (
                Span::from_pair(&name_tok, &tok),
                Some(tok.maybe_to_symbol().unwrap()),
            ),
            _ => (name_tok.span, None),
        }
    };

    Ok((
        input,
        Spanned {
            node: FromImportName { name, alias },
            span,
        },
    ))
}

/// Parse a type description such as "bool", "map<address, uint256>", etc.
pub fn type_desc(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    alt((map_type, base_type))(input)
}

/// Parse a map type such as "map<address, uint256>" etc.
pub fn map_type(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    alt((map_type_double, map_type_single))(input)
}

/// Parse a map type that ends with a right-shift token.
///
/// Example: map<address, map<address, bool>>
pub fn map_type_double(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    let (input, map_kw_1) = name_string("map")(input)?;
    let (input, _) = token(Lt)(input)?;
    let (input, from_1) = base_type(input)?;
    let (input, _) = token(Comma)(input)?;

    let (input, map_kw_2) = name_string("map")(input)?;
    let (input, _) = token(Lt)(input)?;
    let (input, from_2) = base_type(input)?;
    let (input, _) = token(Comma)(input)?;

    let (input, to) = type_desc(input)?;
    let (input, r_brackets) = token(Shr)(input)?;

    let inner_map = Spanned {
        node: TypeDesc::Map {
            from: Box::new(from_2),
            to: Box::new(to),
        },
        span: Span::new(map_kw_2.span.start, r_brackets.span.end - 1),
    };

    Ok((
        input,
        Spanned {
            node: TypeDesc::Map {
                from: Box::new(from_1),
                to: Box::new(inner_map),
            },
            span: Span::from_pair(&map_kw_1, &r_brackets),
        },
    ))
}

/// Parse a map type that ends with a greater-than token.
///
/// Example: map<address, bool>
pub fn map_type_single(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    let (input, map_kw) = name_string("map")(input)?;
    let (input, _) = token(Lt)(input)?;
    let (input, from) = base_type(input)?;
    let (input, _) = token(Comma)(input)?;
    let (input, to) = type_desc(input)?;
    let (input, r_bracket) = token(Gt)(input)?;

    Ok((
        input,
        Spanned {
            node: TypeDesc::Map {
                from: Box::new(from),
                to: Box::new(to),
            },
            span: Span::from_pair(&map_kw, &r_bracket),
        },
    ))
}

/// Parse a base type such as "bool", "uint129[10]", etc.  This does not include
/// map types.
pub fn base_type(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    let (input, base) = name_token(input)?;
    let (input, dims) = arr_list(input)?;

    let mut result = Spanned {
        node: TypeDesc::Base {
            base: base.maybe_to_symbol().unwrap(),
        },
        span: (&base).into(),
    };
    for dim in dims {
        let span = Span::from_pair(&result, &dim);

        result = Spanned {
            node: TypeDesc::Array {
                typ: Box::new(result),
                dimension: dim.node,
            },
            span,
        };
    }

    Ok((input, result))
}

/// Parse an array dimension list such as "[10][2]".
pub fn arr_list(input: Cursor) -> ParseResult<Vec<Spanned<usize>>> {
    many0(arr_dim)(input)
}

/// Parse a single array dimension such as "[10]".
pub fn arr_dim(input: Cursor) -> ParseResult<Spanned<usize>> {
    let (input, l_bracket) = token(OpenBracket)(input)?;
    let (input, num_tok) = num_token(input)?;
    let (input, r_bracket) = token(CloseBracket)(input)?;

    let num_lit = unsafe { input.span_to_string(num_tok.span) };

    let n: usize = match num_lit.parse() {
        Ok(n) => n,
        Err(_) => {
            return Err(ParseError::at_span(
                format!("invalid integer literal \"{}\"", num_lit),
                num_tok.span,
            ))
        }
    };
    if n < 1 {
        return Err(ParseError::at_span(
            "array dimension must be positive".to_string(),
            num_tok.span,
        ));
    }

    Ok((
        input,
        Spanned {
            node: n,
            span: Span::from_pair(&l_bracket, &r_bracket),
        },
    ))
}

/// Parse a dotted import name.
pub fn dotted_name(input: Cursor) -> ParseResult<Spanned<Vec<Symbol>>> {
    let (input, first_part) = name_token(input)?;
    let (input, other_parts) = many0(preceded(token(Dot), name_token))(input)?;

    let mut path = vec![first_part.maybe_to_symbol().unwrap()];
    path.extend(other_parts.iter().map(|t| t.maybe_to_symbol().unwrap()));

    let span = if other_parts.is_empty() {
        first_part.span
    } else {
        let last_part = other_parts.last().unwrap();
        Span::from_pair(&first_part, last_part)
    };

    Ok((input, Spanned { node: path, span }))
}

/// Parse preceding dots used to indicate parent module imports in import
/// statements.
pub fn dots_to_int(input: Cursor) -> ParseResult<Spanned<usize>> {
    let (input, toks) = many1(alt((token(Dot), token(Ellipsis))))(input)?;

    let value = toks
        .iter()
        .map(|t| if t.kind == Dot { 1 } else { 3 })
        .sum::<usize>()
        - 1;

    let span = {
        let first = toks.first().unwrap();
        let last = toks.last().unwrap();

        Span::from_pair(first, last)
    };

    Ok((input, Spanned { node: value, span }))
}

/// Parse a contract definition statement.
pub fn contract_def(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    // "contract" name ":" NEWLINE
    let (input, contract_kw) = name_string("contract")(input)?;
    let (input, name) = name_token(input)?;
    let (input, _) = token(Colon)(input)?;
    let (input, _) = token(Newline)(input)?;

    // INDENT contract_stmt+ DEDENT
    let (input, _) = token(Indent)(input)?;
    let (input, body) = many1(contract_stmt)(input)?;
    let (input, _) = token(Dedent)(input)?;

    let last_stmt = body.last().unwrap();
    let span = Span::from_pair(&contract_kw, last_stmt);

    Ok((
        input,
        Spanned {
            node: ContractDef {
                name: name.maybe_to_symbol().unwrap(),
                body,
            },
            span,
        },
    ))
}

/// Parse a contract statement.
pub fn contract_stmt(input: Cursor) -> ParseResult<Spanned<ContractStmt>> {
    event_def(input)
}

/// Parse an event definition statement.
pub fn event_def(input: Cursor) -> ParseResult<Spanned<ContractStmt>> {
    // "event" name ":" NEWLINE
    let (input, event_kw) = name_string("event")(input)?;
    let (input, name) = name_token(input)?;
    let (input, _) = token(Colon)(input)?;
    let (input, _) = token(Newline)(input)?;

    // INDENT event_field+ DEDENT
    let (input, _) = token(Indent)(input)?;
    let (input, fields) = many1(event_field)(input)?;
    let (input, _) = token(Dedent)(input)?;

    let last_field = fields.last().unwrap();
    let span = Span::from_pair(&event_kw, last_field);

    Ok((
        input,
        Spanned {
            node: ContractStmt::EventDef {
                name: name.maybe_to_symbol().unwrap(),
                fields,
            },
            span,
        },
    ))
}

/// Parse an event field definition.
pub fn event_field(input: Cursor) -> ParseResult<Spanned<EventField>> {
    let (input, name) = name_token(input)?;
    let (input, _) = token(Colon)(input)?;
    let (input, typ) = name_token(input)?;
    let (input, _) = token(Newline)(input)?;

    let span = Span::from_pair(&name, &typ);

    Ok((
        input,
        Spanned {
            node: EventField {
                name: name.maybe_to_symbol().unwrap(),
                typ: (&typ).into(),
            },
            span,
        },
    ))
}

/// Parse a constant expression that can be evaluated at compile-time.
pub fn const_expr(input: Cursor) -> ParseResult<Spanned<ConstExpr>> {
    let (input, head) = const_term(input)?;
    let (input, tail) = many0(alt((
        pair(token(Plus), const_term),
        pair(token(Minus), const_term),
    )))(input)?;

    let mut left_expr = head;
    for (op_tok, right_expr) in tail {
        let span = Span::from_pair(&left_expr, &right_expr);

        left_expr = Spanned {
            node: ConstExpr::BinOp {
                left: Box::new(left_expr),
                op: Operator::try_from(&op_tok).unwrap(),
                right: Box::new(right_expr),
            },
            span,
        };
    }

    Ok((input, left_expr))
}

/// Parse a constant term that may appear as the operand of an addition or
/// subtraction.
pub fn const_term(input: Cursor) -> ParseResult<Spanned<ConstExpr>> {
    let (input, head) = const_factor(input)?;
    let (input, tail) = many0(alt((
        pair(token(Star), const_factor),
        pair(token(Slash), const_factor),
        pair(token(Percent), const_factor),
    )))(input)?;

    let mut left_expr = head;
    for (op_tok, right_expr) in tail {
        let span = Span::from_pair(&left_expr, &right_expr);

        left_expr = Spanned {
            node: ConstExpr::BinOp {
                left: Box::new(left_expr),
                op: Operator::try_from(&op_tok).unwrap(),
                right: Box::new(right_expr),
            },
            span,
        };
    }

    Ok((input, left_expr))
}

/// Parse a constant factor that may appear as the operand of a multiplication,
/// division, modulus, or unary op or as the exponent of a power expression.
pub fn const_factor(input: Cursor) -> ParseResult<Spanned<ConstExpr>> {
    let unary_op = map(
        pair(alt((token(Plus), token(Minus), token(Tilde))), const_factor),
        |res| {
            let (op_tok, operand) = res;
            let span = Span::from_pair(&op_tok, &operand);

            Spanned {
                node: ConstExpr::UnaryOp {
                    op: UnaryOp::try_from(&op_tok).unwrap(),
                    operand: Box::new(operand),
                },
                span,
            }
        },
    );

    alt((unary_op, const_power))(input)
}

/// Parse a constant power expression that may appear in the position of a
/// constant factor.
pub fn const_power(input: Cursor) -> ParseResult<Spanned<ConstExpr>> {
    let bin_op = map(
        separated_pair(const_atom, token(StarStar), const_factor),
        |res| {
            let (left, right) = res;
            let span = Span::from_pair(&left, &right);

            Spanned {
                node: ConstExpr::BinOp {
                    left: Box::new(left),
                    op: Operator::Pow,
                    right: Box::new(right),
                },
                span,
            }
        },
    );

    alt((bin_op, const_atom))(input)
}

/// Parse a constant atom expression that may appear in the position of a
/// constant power or as the base of a constant power expression.
pub fn const_atom(input: Cursor) -> ParseResult<Spanned<ConstExpr>> {
    alt((
        const_group,
        map(name_token, |t| Spanned {
            node: ConstExpr::Name {
                name: t.maybe_to_symbol().unwrap(),
            },
            span: t.span,
        }),
        map(num_token, |t| Spanned {
            node: ConstExpr::Num {
                num: unsafe { input.span_to_string(t.span) },
            },
            span: t.span,
        }),
    ))(input)
}

/// Parse a parenthesized constant group that may appear in the position of a
/// constant atom.
pub fn const_group(input: Cursor) -> ParseResult<Spanned<ConstExpr>> {
    let (input, o_paren) = token(OpenParen)(input)?;
    let (input, spanned_expr) = const_expr(input)?;
    let (input, c_paren) = token(CloseParen)(input)?;

    Ok((
        input,
        Spanned {
            node: spanned_expr.node,
            span: Span::from_pair(&o_paren, &c_paren),
        },
    ))
}
