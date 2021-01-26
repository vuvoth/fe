use crate::yul::names;
use fe_analyzer::namespace::types::Struct;
use yultsur::*;

/// Generate a YUL function that can be used to create an instance of
/// `struct_type`
pub fn generate_new_fn(struct_type: &Struct) -> yul::Statement {
    let function_name = names::struct_api_new(&struct_type.name);

    let params = struct_type
        .fields
        .keys()
        .map(|key| {
            identifier! {(key)}
        })
        .collect::<Vec<_>>();

    let body = struct_type
        .fields
        .keys()
        .enumerate()
        .map(|(index, key)| {
            if index == 0 {
                let param_identifier_exp = identifier_expression! {(key)};
                statements! {
                    (return_val := alloc(32))
                    (mstore(return_val, [param_identifier_exp]))
                }
            } else {
                let ptr_identifier = format!("{}_ptr", key);
                let ptr_identifier = identifier! {(ptr_identifier)};
                let ptr_identifier_exp = identifier_expression! {(ptr_identifier)};
                let param_identifier_exp = identifier_expression! {(key)};
                statements! {
                    (let [ptr_identifier] := alloc(32))
                    (mstore([ptr_identifier_exp], [param_identifier_exp]))
                }
            }
        })
        .flatten()
        .collect::<Vec<_>>();

    function_definition! {
        function [function_name]([params...]) -> return_val {
            [body...]
        }
    }
}

/// Generate a YUL function that can be used to read a property of `struct_type`
pub fn generate_get_fn(struct_type: &Struct, field_name: &str) -> yul::Statement {
    let function_name = names::struct_api_get(&struct_type.name, field_name);
    let fields: Vec<String> = struct_type.fields.keys().cloned().collect();
    // TODO: Fixme
    let field_offset = fields.iter().position(|field| field == field_name).unwrap() * 32;

    let offset = literal_expression! {(field_offset)};
    let return_expression = expression! { add(ptr, [offset]) };
    let body = statement! { (return_val := [return_expression]) };
    function_definition! {
        function [function_name](ptr) -> return_val {
             [body]
        }
    }
}

/// Builds a set of functions used to interact with structs used in a contract
pub fn struct_apis(struct_type: Struct) -> Vec<yul::Statement> {
    [
        vec![generate_new_fn(&struct_type)],
        struct_type
            .fields
            .keys()
            .map(|field| generate_get_fn(&struct_type, field))
            .collect(),
    ]
    .concat()
}

#[cfg(test)]
mod tests {
    use crate::yul::runtime::functions::structs;
    use fe_analyzer::namespace::types::{
        Base,
        Struct,
    };

    // TODO
    // #[test]
    // fn test_empty_struck() {
    //     assert_eq!(
    //         struct_apis(Struct::new("Foo")).to_string(),
    //         "function fuuuk() -> return_val { return_val := alloc(0) }"
    //     )
    // }

    #[test]
    fn test_struct_api_generation() {
        let mut val = Struct::new("Foo");
        val.fields.insert("bar".to_string(), Base::Bool);
        val.fields.insert("bar2".to_string(), Base::Bool);
        assert_eq!(
            structs::generate_new_fn(&val).to_string(),
            "function struct_Foo_new(bar, bar2) -> return_val { return_val := alloc(32) mstore(return_val, bar) let bar2_ptr := alloc(32) mstore(bar2_ptr, bar2) }"
        )
    }

    #[test]
    fn test_struct_getter_generation() {
        let mut val = Struct::new("Foo");
        val.fields.insert("bar".to_string(), Base::Bool);
        val.fields.insert("bar2".to_string(), Base::Bool);
        assert_eq!(
            structs::generate_get_fn(&val, &val.fields.keys().nth(0).unwrap()).to_string(),
            "function struct_Foo_get_bar(ptr) -> return_val { return_val := add(ptr, 0) }"
        );
        assert_eq!(
            structs::generate_get_fn(&val, &val.fields.keys().nth(1).unwrap()).to_string(),
            "function struct_Foo_get_bar2(ptr) -> return_val { return_val := add(ptr, 32) }"
        )
    }
}
