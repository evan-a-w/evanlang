open Types
open Default_types

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
    (StringMap.of_seq (List.to_seq
      [ ("k", Generic_ (TraitSet.of_list ["Hash"]))
      ; ("v", any_type)
      ]
    ))

let type_state_with_hash = add_trait initial_type_state ("Hash", hashable)

let traits_type_state =
  List.fold_left add_trait initial_type_state 
    [ ("Hash", hashable)
    ; ("Display", display)
    ; ("Eq", eq)
    ]
