open Type_sys
open Frontend.Types
open Core
open Types

type parse_state = {
  t_state : Types.type_state;
  v_env : var_env;
  t_map : Frontend.Types.type_lit Types.StringMap.t;
} and var_env = {
  var : Types.typ ref;
  prev : var_env option;
} and type_lit =
  [ `Sum of (string * Types.typ) list
  | `Prod of (string * Types.typ) list ]

let add_type ps (t_expr, t_spec, t_lit) = let open Option.Let_syntax in
  let%bind (param_list, type_name) = match t_expr with
    | `Single x -> Some x
    | _ -> None in
  let generic_list =
    let get_trait_list s =
      Option.value ~default:[]
                   (List.Assoc.find ~equal:String.equal t_spec s) in
    let f s = (s, Types.Generic_ (Types.TraitSet.of_list (get_trait_list s))) in
    List.map ~f:f param_list in
  let type_params = Types.StringMap.of_seq (Stdlib.List.to_seq generic_list) in
  let new_type = Types.Concrete_ { type_name; type_params } in
  Some { ps with t_state = Types.add_type ps.t_state (type_name, new_type);
                 t_map = Types.StringMap.add type_name t_lit ps.t_map;
       }
