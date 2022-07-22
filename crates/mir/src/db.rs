use std::{rc::Rc, sync::Arc};

use fe_analyzer::{
    db::AnalyzerDbStorage,
    namespace::{items as analyzer_items, types as analyzer_types},
    AnalyzerDb,
};
use fe_common::db::{SourceDb, SourceDbStorage, Upcast, UpcastMut};

use crate::ir::{self, ConstantId, TypeId};

mod queries;

#[salsa::query_group(MirDbStorage)]
pub trait MirDb: AnalyzerDb + Upcast<dyn AnalyzerDb> + UpcastMut<dyn AnalyzerDb> {
    #[salsa::interned]
    fn mir_intern_const(&self, data: Arc<ir::Constant>) -> ir::ConstantId;
    #[salsa::interned]
    fn mir_intern_type(&self, data: Arc<ir::Type>) -> ir::TypeId;
    #[salsa::interned]
    fn mir_intern_function(&self, data: Arc<ir::FunctionSignature>) -> ir::FunctionId;

    #[salsa::invoke(queries::module::mir_lower_module_all_functions)]
    fn mir_lower_module_all_functions(
        &self,
        module: analyzer_items::ModuleId,
    ) -> Rc<Vec<ir::FunctionId>>;

    #[salsa::invoke(queries::contract::mir_lower_contract_all_functions)]
    fn mir_lower_contract_all_functions(
        &self,
        contract: analyzer_items::ContractId,
    ) -> Rc<Vec<ir::FunctionId>>;

    #[salsa::invoke(queries::structs::mir_lower_struct_all_functions)]
    fn mir_lower_struct_all_functions(
        &self,
        struct_: analyzer_items::StructId,
    ) -> Rc<Vec<ir::FunctionId>>;

    #[salsa::invoke(queries::types::mir_lowered_type)]
    fn mir_lowered_type(&self, analyzer_type: analyzer_types::TypeId) -> TypeId;
    #[salsa::invoke(queries::types::mir_lowered_event_type)]
    fn mir_lowered_event_type(&self, analyzer_type: analyzer_items::EventId) -> TypeId;

    #[salsa::invoke(queries::constant::mir_lowered_constant)]
    fn mir_lowered_constant(&self, analyzer_const: analyzer_items::ModuleConstantId) -> ConstantId;

    #[salsa::invoke(queries::function::mir_lowered_func_signature)]
    fn mir_lowered_func_signature(
        &self,
        analyzer_func: analyzer_items::FunctionId,
    ) -> ir::FunctionId;
    #[salsa::invoke(queries::function::mir_lowered_monomorphized_func_signature)]
    fn mir_lowered_monomorphized_func_signature(
        &self,
        analyzer_func: analyzer_items::FunctionId,
        concrete_args: Vec<analyzer_types::TypeId>,
    ) -> ir::FunctionId;
    #[salsa::invoke(queries::function::mir_lowered_func_body)]
    fn mir_lowered_func_body(&self, func: ir::FunctionId) -> Rc<ir::FunctionBody>;
}

#[salsa::database(SourceDbStorage, AnalyzerDbStorage, MirDbStorage)]
#[derive(Default)]
pub struct NewDb {
    storage: salsa::Storage<NewDb>,
}
impl salsa::Database for NewDb {}

impl Upcast<dyn SourceDb> for NewDb {
    fn upcast(&self) -> &(dyn SourceDb + 'static) {
        &*self
    }
}

impl UpcastMut<dyn SourceDb> for NewDb {
    fn upcast_mut(&mut self) -> &mut (dyn SourceDb + 'static) {
        &mut *self
    }
}

impl Upcast<dyn AnalyzerDb> for NewDb {
    fn upcast(&self) -> &(dyn AnalyzerDb + 'static) {
        &*self
    }
}

impl UpcastMut<dyn AnalyzerDb> for NewDb {
    fn upcast_mut(&mut self) -> &mut (dyn AnalyzerDb + 'static) {
        &mut *self
    }
}
