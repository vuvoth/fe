use crate::context::{Analysis, AnalyzerContext, Constant};
use crate::display::Displayable;
use crate::errors::{self, ConstEvalError, TypeError};
use crate::namespace::items::{
    Contract, ContractId, Event, Function, Impl, ImplId, Item, ModuleConstant, ModuleConstantId,
    ModuleId, ModuleSource, Struct, StructId, Trait, TraitId, TypeAlias, TypeDef,
};
use crate::namespace::scopes::ItemScope;
use crate::namespace::types::{self, TypeId};
use crate::traversal::{const_expr, expressions, types::type_desc};
use crate::AnalyzerDb;
use fe_common::diagnostics::Label;
use fe_common::files::Utf8Path;
use fe_common::Span;
use fe_parser::{ast, node::Node};
use indexmap::indexmap;
use indexmap::map::{Entry, IndexMap};
use smol_str::SmolStr;
use std::rc::Rc;
use std::sync::Arc;

pub fn module_file_path(db: &dyn AnalyzerDb, module: ModuleId) -> SmolStr {
    let full_path = match &module.data(db).source {
        ModuleSource::File(file) => file.path(db.upcast()).as_str().into(),
        ModuleSource::Dir(path) => path.clone(),
    };

    let src_prefix = &module.ingot(db).data(db).src_dir;

    Utf8Path::new(full_path.as_str())
        .strip_prefix(src_prefix.as_str())
        .map(|path| path.as_str().into())
        .unwrap_or(full_path)
}

pub fn module_parse(db: &dyn AnalyzerDb, module: ModuleId) -> Analysis<Arc<ast::Module>> {
    let data = module.data(db);
    match data.source {
        ModuleSource::File(file) => {
            let (ast, diags) = fe_parser::parse_file(file, &file.content(db.upcast()));
            Analysis::new(ast.into(), diags.into())
        }
        ModuleSource::Dir(_) => {
            // Directory with no corresponding source file. Return empty ast.
            Analysis::new(ast::Module { body: vec![] }.into(), vec![].into())
        }
    }
}

pub fn module_is_incomplete(db: &dyn AnalyzerDb, module: ModuleId) -> bool {
    if matches!(module.data(db).source, ModuleSource::File(_)) {
        let ast = module.ast(db);
        ast.body
            .last()
            .map(|stmt| matches!(stmt, ast::ModuleStmt::ParseError(_)))
            .unwrap_or(false)
    } else {
        false
    }
}

pub fn module_all_items(db: &dyn AnalyzerDb, module: ModuleId) -> Arc<[Item]> {
    let body = &module.ast(db).body;
    body.iter()
        .filter_map(|stmt| match stmt {
            ast::ModuleStmt::TypeAlias(node) => Some(Item::Type(TypeDef::Alias(
                db.intern_type_alias(Arc::new(TypeAlias {
                    ast: node.clone(),
                    module,
                })),
            ))),
            ast::ModuleStmt::Contract(node) => Some(Item::Type(TypeDef::Contract(
                db.intern_contract(Arc::new(Contract {
                    name: node.name().into(),
                    ast: node.clone(),
                    module,
                })),
            ))),
            ast::ModuleStmt::Struct(node) => Some(Item::Type(TypeDef::Struct(db.intern_struct(
                Arc::new(Struct {
                    ast: node.clone(),
                    module,
                }),
            )))),
            ast::ModuleStmt::Constant(node) => Some(Item::Constant(db.intern_module_const(
                Arc::new(ModuleConstant {
                    ast: node.clone(),
                    module,
                }),
            ))),
            ast::ModuleStmt::Function(node) => Some(Item::Function(
                db.intern_function(Arc::new(Function::new(db, node, None, module))),
            )),
            ast::ModuleStmt::Trait(node) => Some(Item::Trait(db.intern_trait(Arc::new(Trait {
                ast: node.clone(),
                module,
            })))),
            ast::ModuleStmt::Pragma(_) | ast::ModuleStmt::Use(_) | ast::ModuleStmt::Impl(_) => None,
            ast::ModuleStmt::Event(node) => Some(Item::Event(db.intern_event(Arc::new(Event {
                ast: node.clone(),
                module,
                contract: None,
            })))),
            ast::ModuleStmt::ParseError(_) => None,
        })
        .collect()
}

pub fn module_all_impls(db: &dyn AnalyzerDb, module: ModuleId) -> Arc<[ImplId]> {
    let body = &module.ast(db).body;
    body.iter()
        .filter_map(|stmt| match stmt {
            ast::ModuleStmt::Impl(impl_node) => {
                let treit = module
                    .items(db)
                    .get(&impl_node.kind.impl_trait.kind)
                    .cloned();

                let mut scope = ItemScope::new(db, module);
                let receiver_type = type_desc(&mut scope, &impl_node.kind.receiver).unwrap();

                if let Some(Item::Trait(val)) = treit {
                    Some(db.intern_impl(Arc::new(Impl {
                        trait_id: val,
                        receiver: receiver_type,
                        ast: impl_node.clone(),
                        module,
                    })))
                } else {
                    None
                }
            }
            _ => None,
        })
        .collect()
}

pub fn module_item_map(
    db: &dyn AnalyzerDb,
    module: ModuleId,
) -> Analysis<Arc<IndexMap<SmolStr, Item>>> {
    // we must check for conflicts with global item names
    let global_items = module.global_items(db);

    // sub modules and used items are included in this map
    let submodules = module
        .submodules(db)
        .iter()
        .map(|id| (id.name(db), Item::Module(*id)))
        .collect::<IndexMap<_, _>>();
    let used_items = db.module_used_item_map(module);

    let mut diagnostics = used_items.diagnostics.to_vec();
    let mut map = IndexMap::<SmolStr, Item>::new();

    for item in module.all_items(db).iter() {
        let item_name = item.name(db);
        if let Some(global_item) = global_items.get(&item_name) {
            let kind = item.item_kind_display_name();
            let other_kind = global_item.item_kind_display_name();
            diagnostics.push(errors::error(
                &format!(
                    "{} name conflicts with the {} named \"{}\"",
                    kind, other_kind, item_name
                ),
                item.name_span(db)
                    .expect("user defined item is missing a name span"),
                &format!("`{}` is already defined", item_name),
            ));
            continue;
        }

        if let Some((used_item_name_span, used_item)) = used_items.value.get(&item_name) {
            diagnostics.push(errors::duplicate_name_error(
                &format!(
                    "a {} with the same name has already been imported",
                    used_item.item_kind_display_name()
                ),
                &item.name(db),
                *used_item_name_span,
                item.name_span(db).expect("missing name span"),
            ));
            continue;
        }

        match map.entry(item_name.clone()) {
            Entry::Occupied(entry) => {
                if let Some(entry_name_span) = entry.get().name_span(db) {
                    diagnostics.push(errors::duplicate_name_error(
                        &format!(
                            "a {} named \"{}\" has already been defined",
                            entry.get().item_kind_display_name(),
                            item_name
                        ),
                        &item_name,
                        entry_name_span,
                        item.name_span(db)
                            .expect("used-defined item does not have name span"),
                    ));
                } else {
                    diagnostics.push(errors::fancy_error(
                        &format!(
                            "a {} named \"{}\" has already been defined",
                            entry.get().item_kind_display_name(),
                            item_name
                        ),
                        vec![Label::primary(
                            item.name_span(db)
                                .expect("used-defined item does not have name span"),
                            format!("`{}` redefined here", entry.key()),
                        )],
                        vec![],
                    ));
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(*item);
            }
        }
    }
    Analysis::new(
        map.into_iter()
            .chain(submodules)
            .chain(
                used_items
                    .value
                    .iter()
                    .map(|(name, (_, item))| (name.clone(), *item)),
            )
            .collect::<IndexMap<_, _>>()
            .into(),
        diagnostics.into(),
    )
}

pub fn module_impl_map(
    db: &dyn AnalyzerDb,
    module: ModuleId,
) -> Analysis<Arc<IndexMap<(TraitId, TypeId), ImplId>>> {
    let scope = ItemScope::new(db, module);
    let mut map = IndexMap::<(TraitId, TypeId), ImplId>::new();

    for impl_ in db.module_all_impls(module).iter() {
        let key = &(impl_.trait_id(db), impl_.receiver(db));

        match map.entry(*key) {
            Entry::Occupied(entry) => {
                scope.duplicate_name_error(
                    &format!(
                        "duplicate `impl` blocks for trait `{}` for type `{}`",
                        key.0.name(db),
                        key.1.display(db)
                    ),
                    "",
                    entry.get().ast(db).span,
                    impl_.ast(db).span,
                );
            }
            Entry::Vacant(entry) => {
                entry.insert(*impl_);
            }
        }
    }
    Analysis::new(Arc::new(map), scope.diagnostics.take().into())
}

pub fn module_contracts(db: &dyn AnalyzerDb, module: ModuleId) -> Arc<[ContractId]> {
    module
        .all_items(db)
        .iter()
        .filter_map(|item| match item {
            Item::Type(TypeDef::Contract(id)) => Some(*id),
            _ => None,
        })
        .collect()
}

pub fn module_structs(db: &dyn AnalyzerDb, module: ModuleId) -> Arc<[StructId]> {
    module
        .all_items(db)
        .iter()
        .chain(
            module
                .used_items(db)
                .values()
                .into_iter()
                .map(|(_, item)| item),
        )
        .filter_map(|item| match item {
            Item::Type(TypeDef::Struct(id)) => Some(*id),
            _ => None,
        })
        .collect()
}

pub fn module_constants(db: &dyn AnalyzerDb, module: ModuleId) -> Arc<Vec<ModuleConstantId>> {
    Arc::new(
        module
            .all_items(db)
            .iter()
            .filter_map(|item| match item {
                Item::Constant(id) => Some(*id),
                _ => None,
            })
            .collect(),
    )
}

pub fn module_constant_type(
    db: &dyn AnalyzerDb,
    constant: ModuleConstantId,
) -> Analysis<Result<types::TypeId, TypeError>> {
    let constant_data = constant.data(db);
    let mut scope = ItemScope::new(db, constant.data(db).module);
    let typ = type_desc(&mut scope, &constant_data.ast.kind.typ);

    match &typ {
        Ok(typ) if !typ.is_base(db) => {
            scope.error(
                "Non-base types not yet supported for constants",
                constant.data(db).ast.kind.typ.span,
                &format!(
                    "this has type `{}`; expected a primitive type",
                    typ.display(db)
                ),
            );
        }
        Ok(typ) => {
            if let Ok(expr_attr) =
                expressions::assignable_expr(&mut scope, &constant_data.ast.kind.value, Some(*typ))
            {
                if typ != &expr_attr.typ {
                    scope.type_error(
                        "type mismatch",
                        constant_data.ast.kind.value.span,
                        *typ,
                        expr_attr.typ,
                    );
                }
            }
        }
        _ => {}
    }

    Analysis::new(typ, scope.diagnostics.take().into())
}

pub fn module_constant_type_cycle(
    db: &dyn AnalyzerDb,
    _cycle: &[String],
    constant: &ModuleConstantId,
) -> Analysis<Result<TypeId, TypeError>> {
    let mut context = ItemScope::new(db, constant.data(db).module);
    let err = Err(TypeError::new(context.error(
        "recursive constant value definition",
        constant.data(db).ast.span,
        "",
    )));

    Analysis {
        value: err,
        diagnostics: context.diagnostics.take().into(),
    }
}

pub fn module_constant_value(
    db: &dyn AnalyzerDb,
    constant: ModuleConstantId,
) -> Analysis<Result<Constant, ConstEvalError>> {
    let constant_data = constant.data(db);

    // Create `ItemScope` to collect expression types for constant evaluation.
    // TODO: Consider whether it's better to run semantic analysis twice(first
    // analysis is already done in `module_constant_type`) or cache expression
    // types in salsa.
    let mut scope = ItemScope::new(db, constant.data(db).module);
    let typ = match type_desc(&mut scope, &constant_data.ast.kind.typ) {
        Ok(typ) => typ,
        // No need to emit diagnostics, it's already emitted in `module_constant_type`.
        Err(err) => {
            return Analysis {
                value: Err(err.into()),
                diagnostics: vec![].into(),
            };
        }
    };

    if let Err(err) =
        expressions::assignable_expr(&mut scope, &constant_data.ast.kind.value, Some(typ))
    {
        // No need to emit diagnostics, it's already emitted in `module_constant_type`.
        return Analysis {
            value: Err(err.into()),
            diagnostics: vec![].into(),
        };
    }

    // Clear diagnostics emitted from `module_constant_type`.
    scope.diagnostics.borrow_mut().clear();

    // Perform constant evaluation.
    let value = const_expr::eval_expr(&mut scope, &constant_data.ast.kind.value);

    Analysis {
        value,
        diagnostics: scope.diagnostics.take().into(),
    }
}

pub fn module_constant_value_cycle(
    db: &dyn AnalyzerDb,
    _cycle: &[String],
    constant: &ModuleConstantId,
) -> Analysis<Result<Constant, ConstEvalError>> {
    let mut context = ItemScope::new(db, constant.data(db).module);
    let err = Err(ConstEvalError::new(context.error(
        "recursive constant value definition",
        constant.data(db).ast.span,
        "",
    )));

    Analysis {
        value: err,
        diagnostics: context.diagnostics.take().into(),
    }
}

pub fn module_used_item_map(
    db: &dyn AnalyzerDb,
    module: ModuleId,
) -> Analysis<Arc<IndexMap<SmolStr, (Span, Item)>>> {
    // we must check for conflicts with the global items map
    let global_items = module.global_items(db);

    let mut diagnostics = vec![];
    let body = &module.ast(db).body;

    let items = body
        .iter()
        .fold(indexmap! {}, |mut accum, stmt| {
            if let ast::ModuleStmt::Use(use_stmt) = stmt {
                let items = resolve_use_tree(db, module, &use_stmt.kind.tree, true);
                diagnostics.extend(items.diagnostics.iter().cloned());

                for (name, (name_span, item)) in items.value.iter() {
                    if let Some((other_name_span, other_item)) =
                        accum.insert(name.clone(), (*name_span, *item))
                    {
                        diagnostics.push(errors::duplicate_name_error(
                            &format!(
                                "a {} with the same name has already been imported",
                                other_item.item_kind_display_name()
                            ),
                            name,
                            other_name_span,
                            *name_span,
                        ));
                    }
                }
            }

            accum
        })
        .into_iter()
        .filter_map(|(name, (name_span, item))| {
            if let Some(global_item) = global_items.get(&name) {
                let other_kind = global_item.item_kind_display_name();

                diagnostics.push(errors::error(
                    &format!(
                        "import name conflicts with the {} named \"{}\"",
                        other_kind, name
                    ),
                    name_span,
                    &format!("`{}` is already defined", name),
                ));

                None
            } else {
                Some((name, (name_span, item)))
            }
        })
        .collect::<IndexMap<_, _>>();

    Analysis::new(Arc::new(items), diagnostics.into())
}

pub fn module_parent_module(db: &dyn AnalyzerDb, module: ModuleId) -> Option<ModuleId> {
    module
        .ingot(db)
        .all_modules(db)
        .iter()
        .find(|&&id| id != module && id.submodules(db).iter().any(|&sub| sub == module))
        .copied()
}

pub fn module_submodules(db: &dyn AnalyzerDb, module: ModuleId) -> Arc<[ModuleId]> {
    // The module tree is entirely based on the file hierarchy for now.

    let ingot = module.ingot(db);
    if Some(module) == ingot.root_module(db) {
        ingot
            .all_modules(db)
            .iter()
            .copied()
            .filter(|&module_id| {
                module_id != module
                    && Utf8Path::new(module_id.file_path_relative_to_src_dir(db).as_str())
                        .components()
                        .take(2)
                        .count()
                        == 1
            })
            .collect()
    } else {
        let dir_path = match &module.data(db).source {
            ModuleSource::Dir(path) => path.as_str().into(),
            _ => {
                let file_path = module.file_path_relative_to_src_dir(db);
                let path = Utf8Path::new(file_path.as_str());
                path.parent()
                    .unwrap_or_else(|| Utf8Path::new(""))
                    .join(path.file_stem().expect("source file name with no stem"))
            }
        };

        ingot
            .all_modules(db)
            .iter()
            .copied()
            .filter(|&module_id| {
                module_id != module
                    && Utf8Path::new(module_id.file_path_relative_to_src_dir(db).as_str())
                        .parent()
                        .unwrap_or_else(|| {
                            panic!(
                                "module file in ingot does not have parent path: `{}`",
                                module_id.file_path_relative_to_src_dir(db)
                            )
                        })
                        == dir_path
            })
            .collect()
    }
}

/// Resolve a use tree entirely. We set internal to true if the first path item
/// is internal.
///
/// e.g. `foo::bar::{baz::bing}`
///       ---        ---
///        ^          ^ baz is not internal
///        foo is internal
fn resolve_use_tree(
    db: &dyn AnalyzerDb,
    module: ModuleId,
    tree: &Node<ast::UseTree>,
    internal: bool,
) -> Analysis<Rc<IndexMap<SmolStr, (Span, Item)>>> {
    let mut diagnostics = vec![];

    // Again, the path resolution method we use depends on whether or not the first
    // item is internal.
    let resolve_path = |module: ModuleId, db: &dyn AnalyzerDb, path: &ast::Path| {
        if internal {
            module.resolve_path_non_used_internal(db, path)
        } else {
            module.resolve_path(db, path)
        }
    };

    match &tree.kind {
        ast::UseTree::Glob { prefix } => {
            let prefix_module = resolve_path(module, db, prefix);
            diagnostics.extend(prefix_module.diagnostics.iter().cloned());

            let items = match prefix_module.value {
                Some(Item::Module(module)) => module
                    .items(db)
                    .iter()
                    .map(|(name, item)| (name.clone(), (tree.span, *item)))
                    .collect(),
                Some(item) => {
                    diagnostics.push(errors::error(
                        format!("cannot glob import from {}", item.item_kind_display_name()),
                        prefix.segments.last().expect("path is empty").span,
                        "prefix item must be a module",
                    ));
                    indexmap! {}
                }
                None => indexmap! {},
            };

            Analysis::new(items.into(), diagnostics.into())
        }
        ast::UseTree::Nested { prefix, children } => {
            let prefix_module = resolve_path(module, db, prefix);
            diagnostics.extend(prefix_module.diagnostics.iter().cloned());

            let items = match prefix_module.value {
                Some(Item::Module(module)) => {
                    children.iter().fold(indexmap! {}, |mut accum, node| {
                        let child_items = resolve_use_tree(db, module, node, false);
                        diagnostics.extend(child_items.diagnostics.iter().cloned());

                        for (name, (name_span, item)) in child_items.value.iter() {
                            if let Some((other_name_span, other_item)) =
                                accum.insert(name.clone(), (*name_span, *item))
                            {
                                diagnostics.push(errors::duplicate_name_error(
                                    &format!(
                                        "a {} with the same name has already been imported",
                                        other_item.item_kind_display_name()
                                    ),
                                    name,
                                    other_name_span,
                                    *name_span,
                                ));
                            }
                        }

                        accum
                    })
                }
                Some(item) => {
                    diagnostics.push(errors::error(
                        format!("cannot glob import from {}", item.item_kind_display_name()),
                        prefix.segments.last().unwrap().span,
                        "prefix item must be a module",
                    ));
                    indexmap! {}
                }
                None => indexmap! {},
            };

            Analysis::new(items.into(), diagnostics.into())
        }
        ast::UseTree::Simple { path, rename } => {
            let item = resolve_path(module, db, path);

            let items = match item.value {
                Some(item) => {
                    let (item_name, item_name_span) = if let Some(name) = rename {
                        (name.kind.clone(), name.span)
                    } else {
                        let name_segment_node = path.segments.last().expect("path is empty");
                        (name_segment_node.kind.clone(), name_segment_node.span)
                    };

                    indexmap! { item_name => (item_name_span, item) }
                }
                None => indexmap! {},
            };

            Analysis::new(items.into(), item.diagnostics)
        }
    }
}
