open Types

let empty_type_state = {
  trait_impls = [];
  trait_map = StringMap.empty;
  type_map = StringMap.empty;
  type's_traits = [];
}

let construct_type type_name type_params = { type_name; type_params }

let construct_type_no_params name = construct_type name StringMap.empty

let any_type = Generic_ TraitSet.empty

let int_type = construct_type_no_params "int"
let bool_type = construct_type_no_params "bool"
let string_type = construct_type_no_params "String"
let unit_type = construct_type_no_params "unit"
let double_type = construct_type_no_params "double"
let array_type =
  construct_type "Array"
                 (StringMap.of_seq (List.to_seq [("a", any_type)]))
let list_type =
  construct_type "List"
                 (StringMap.of_seq (List.to_seq [("a", any_type)]))

let self_type = construct_type_no_params "Self"

let initial_type_state =
  let f t = (t.type_name, Concrete_ t) in
  List.fold_left add_type empty_type_state 
    (List.map f [ int_type
                ; bool_type
                ; string_type
                ; unit_type
                ; double_type
                ; array_type
                ; list_type
                ])
