use crate::yul::names;
use fe_analyzer::namespace::types::{
    Contract,
    Struct,
};
use yultsur::*;

/// Make a call to a contract of the given type and address with a set of
/// parameters.
pub fn contract_call(
    contract: Contract,
    func_name: String,
    address: yul::Expression,
    params: Vec<yul::Expression>,
) -> yul::Expression {
    let func_name = names::contract_call(&contract.name, &func_name);
    expression! { [func_name]([address], [params...]) }
}

pub fn struct_new_call(struct_type: &Struct, params: Vec<yul::Expression>) -> yul::Expression {
    let function_name = names::struct_api_new(&struct_type.name);
    expression! { [function_name]([params...]) }
}

pub fn struct_get_call(struct_type: &Struct, ptr_name: &str, field_name: &str) -> yul::Expression {
    let function_name = names::struct_api_get(&struct_type.name, field_name);
    let ptr_name_exp = identifier_expression! {(ptr_name)};
    expression! { [function_name]([ptr_name_exp]) }
}

#[cfg(test)]
mod tests {
    use crate::yul::operations::calls;
    use fe_analyzer::namespace::types::{
        Base,
        Struct,
    };
    use yultsur::*;

    #[test]
    fn test_struct_new_call() {
        let mut val = Struct::new("Foo");
        val.fields.insert("bar".to_string(), Base::Bool);
        val.fields.insert("bar2".to_string(), Base::Bool);
        let params = vec![
            identifier_expression! { (1) },
            identifier_expression! { (2) },
        ];
        assert_eq!(
            calls::struct_new_call(&val, params).to_string(),
            "struct_Foo_new(1, 2)"
        )
    }
}
