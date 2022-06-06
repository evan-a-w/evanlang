open Frontend
open Type_sys
open Type_sys.Defaults
open Core

let type_from_t_expr p_state = function
  | `Single (params, name) ->
      let%bind { t_params; t_lit } = Types.StringMap.find_opt name p_state.t_map in
      
  | `Multi l ->

let rec define_types p_state = function
  | `DefType (t_expr, t_spec, t_lit) ->
      let type_added = add_type p_state (t_expr, t_spec, t_lit) in
      type_added, None
  | x -> p_state, Some x

let rec generate_initial_types p_state = let open Option.Let_syntax in
  let fold_state seq state = Parse_state.folding_map_option seq ~f:generate_initial_types ~init:state in
  let of_list = Base.Sequence.of_list in
  let to_list = Base.Sequence.to_list in
  function
  | `Unit -> (p_state, (`Unit, Types.Concrete_ unit_type)) |> Option.some
  | `Bool b -> (p_state, (`Bool b, Types.Concrete_ bool_type)) |> Option.some
  | `FloatLit f -> (p_state, (`FloatLit f, Types.Concrete_ float_type)) |> Option.some
  | `IntLit i -> (p_state, (`IntLit i, Types.Concrete_ int_type)) |> Option.some
  | `StringLit s -> (p_state, (`StringLit s, Types.Concrete_ string_type)) |> Option.some
  | `SymbolLit s -> (p_state, (`SymbolLit s, Types.Concrete_ symbol_type)) |> Option.some
  | `Identifier i -> (p_state, (`Identifier i, any_type)) |> Option.some
  | `ListLit l ->
      let%bind new_p_state, mapped_seq = fold_state (of_list l) p_state in
      let mapped_list = to_list mapped_seq in
      Some (new_p_state, (`ListLit mapped_list, Types.Concrete_ list_type))
  | `ArrayLit a ->
      let seq = Stdlib.Array.to_seq a |> Base.Sequence.of_seq in
      let%bind new_p_state, mapped_seq = fold_state seq p_state in
      let mapped_arr = Base.Sequence.to_seq mapped_seq |> Stdlib.Array.of_seq in
      Some (new_p_state, (`ArrayLit mapped_arr, Types.Concrete_ array_type))
  | `Call (f, l) ->
    let%bind new_p_state, tf = generate_initial_types p_state f in
    let _, t = tf in
    let open Base.Sequence in
    Option.bind (fold_state (of_list l) new_p_state) ~f:(fun (newer_p_state, mapped_seq) ->
    let mapped_list = to_list mapped_seq in
    Some (newer_p_state, (`Call (tf, mapped_list), t)))
  | `DefType (t_expr, t_spec, t_lit) ->
      let type_added = add_type p_state (t_expr, t_spec, t_lit) in
      Some (type_added, (`Unit, Types.Concrete_ unit_type))
  | `Typed (t_expr, e) ->
      let%bind new_p_state, (te, _) = generate_initial_types p_state e in
      Some (new_p_state, (te, type_from_t_expr t_expr))
  | `Let (name, e) -> Some (p_state, (`Unit, Types.Concrete_ unit_type))
  | `Function (args, e) -> Some (p_state, (`Unit, Types.Concrete_ unit_type))
