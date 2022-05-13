open Types
open Default_types
open! Base

let hashable = 
  [Member {
    name = "hash"; 
    typ = Function_ [Concrete_ self_type; Concrete_ int_type]
  }]

let display = 
  [Member {
    name = "to_string"; 
    typ = Function_ [Concrete_ self_type; Concrete_ string_type]
  }]

let eq =
  [Member {
    name = "eq"; 
    typ = Function_ [Concrete_ self_type; Concrete_ self_type; Concrete_ bool_type]
  }]

let map_type =
  construct_type
    "Map"
    (StringMap.of_seq (Stdlib.List.to_seq
      [ ("k", Generic_ (TraitSet.of_list ["Hash"]))
      ; ("v", any_type)
      ]
    ))

let type_state_with_hash = add_trait initial_type_state ("Hash", hashable)

let traits_type_state =
  List.fold_left ~f:add_trait ~init:initial_type_state 
    [ ("Hash", hashable)
    ; ("Display", display)
    ; ("Eq", eq)
    ]

(*
impl Eq for List of a where a: Eq
 *)
let eq_type = Generic_ (TraitSet.of_seq (Stdlib.List.to_seq ["Eq"]))
let eq_list = 
  construct_type "List"
                 (StringMap.of_seq (Stdlib.List.to_seq [("a", eq_type)]))
  |> fun x -> Concrete_ x
let traits_with_eq_list_impl = add_impl traits_type_state ("Eq", eq_list)
let traits_with_eq_int_impl = add_impl traits_with_eq_list_impl ("Eq", Concrete_ int_type)

let test_state = traits_with_eq_int_impl

let%test "int_eq" = 
  TraitSet.equal (get_traits test_state (Concrete_ int_type))
                 (TraitSet.of_seq (Stdlib.List.to_seq ["Eq"]))
