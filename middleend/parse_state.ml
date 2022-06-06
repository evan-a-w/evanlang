open Type_sys
open Frontend.Types
open Core
open Types

type parse_state = {
  t_state : Types.type_state;
  v_env : var_env;
  t_map : stored_type Types.StringMap.t;
} and var_env = {
  var : Types.typ ref;
  prev : var_env option;
} and stored_type = {
  params: (string * (string list)) list;
  type_lit: Frontend.Types.type_lit;
}

let add_type ps (t_expr, t_spec, t_lit) = let open Option.Let_syntax in
  let%bind (param_list, type_name) = match t_expr with
    | `Single x -> Some x
    | _ -> None in
  let traits_list =
    let get_trait_list s =
      Option.value ~default:[]
                   (List.Assoc.find ~equal:String.equal t_spec s) in
    let f s = (s, get_trait_list s) in
    List.map ~f:f param_list in
  let map_generic_type = List.map ~f:(fun (s, trait_list) ->
                                  (s, Types.Generic_ (Types.TraitSet.of_list trait_list))) in
  let generic_list = map_generic_type traits_list in
  let type_params = Types.StringMap.of_seq (Stdlib.List.to_seq generic_list) in
  let new_type = Types.Concrete_ { type_name; type_params } in
  Some { ps with t_state = Types.add_type ps.t_state (type_name, new_type);
                 t_map = Types.StringMap.add type_name { type_lit = t_lit; params = traits_list } ps.t_map;
       }

let folding_map_option :
  exp Base.Sequence.t
  -> init:parse_state
  -> f:(parse_state -> exp -> (parse_state * Middle_types.middle_exp) option)
  -> (parse_state * Middle_types.middle_exp Base.Sequence.t) option
  = fun seq ~init ~f ->
  let unit_val = `Unit, Type_sys.(Types.Concrete_ Defaults.unit_type) in
  let state_ref = ref (Some init) in
  let new_f v = let bound = Option.bind !state_ref ~f:(fun x ->
      match f x v with
      | Some (new_state, res) ->
        (state_ref := Some new_state;
         Some res)
      | None ->
        (state_ref := None;
         Some unit_val)) in
    Option.value bound ~default:unit_val in
  let res = Sequence.map seq ~f:new_f in
  match !state_ref with
  | None -> None
  | Some x -> Some (x, res)
