open Types
open Defaults
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

let%test "list_eq" = 
  TraitSet.equal (get_traits test_state eq_list)
                 (TraitSet.of_seq (Stdlib.List.to_seq ["Eq"]))

let%test "useless_impl" =
  let useless_trait = add_trait test_state ("Useless", []) in
  let new_test_state = add_impl useless_trait ("Useless", Generic_ (TraitSet.of_list ["Eq"])) in
  TraitSet.equal (get_traits new_test_state eq_list)
                 (TraitSet.of_seq (Stdlib.List.to_seq ["Eq"; "Useless"]))

let%test "list_list_eq" = 
  let eq_list_list = 
    construct_type "List"
                   (StringMap.of_seq (Stdlib.List.to_seq [("a", eq_list)])) in
  TraitSet.equal (get_traits test_state (Concrete_ eq_list_list))
                 (TraitSet.of_seq (Stdlib.List.to_seq ["Eq"]))

let%test "any" = 
  let any_traits = get_traits test_state any_type in
  TraitSet.equal any_traits
                 (TraitSet.of_seq (Stdlib.List.to_seq []))

let%test "list_any" = 
  let list_any_traits = get_traits test_state (Concrete_ list_type) in
  TraitSet.equal list_any_traits
                 (TraitSet.of_seq (Stdlib.List.to_seq []))
