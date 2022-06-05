open Frontend
open Type_sys
open Type_sys.Defaults
open Core

let rec generate_initial_types p_state = let open Option.Let_syntax in
  let fold_state seq state = Parse_state.folding_map_option seq ~f:generate_initial_types ~init:state in
  let of_list = Base.Sequence.of_list in
  let to_list = Base.Sequence.to_list in
  function
  | `Unit -> p_state, (`Unit, Types.Concrete_ unit_type) |> Option.some
  | `Bool b -> p_state, (`Bool b, Types.Concrete_ bool_type) |> Option.some
  | `FloatLit f -> p_state, (`Float f, Types.Concrete_ float_type) |> Option.some
  | `IntLit i -> p_state, (`IntLit i, Types.Concrete_ int_type) |> Option.some
  | `StringLit s -> p_state, (`StringLit s, Types.Concrete_ string_type) |> Option.some
  | `SymbolLit s -> p_state, (`SymbolLit s, Types.Concrete_ symbol_type) |> Option.some
  | `Identifier i -> p_state, (`Identifier i, any_type) |> Option.some
  | `ListLit l ->
      let%bind new_p_state, mapped_seq = fold_state (of_list l) p_state in
      let mapped_list = to_list mapped_seq in
      Some (new_p_state, (`ListLit mapped_list, Types.Concrete_ list_type))
  | `ArrayLit a -> Some p_state, (`ArrayLit (Array.map ~f:gwithp a), Types.Concrete_ array_type)
  | `Call (f, l) ->
    let new_opt_p_state, tf = generate_initial_types p_state f in
    let%bind new_p_state = new_opt_p_state in
    let _, t = tf in
    let open Base.Sequence in
    let newer_opt_p_state, mapped_seq = fold_state (of_list l) new_p_state in
    let%bind newer_p_state = newer_opt_p_state in
    (new_p_state, (`Call (tf, List.map ~f:gwithp l), t))
  | `DefType (t_expr, t_spec, t_lit) -> Some p_state, (`Unit, Types.Concrete_ unit_type)
  | `Typed (t_expr, e) -> Some p_state, (`Unit, Types.Concrete_ unit_type)
  | `Let (name, e) -> Some p_state, (`Unit, Types.Concrete_ unit_type)
  | `Function (args, e) -> Some p_state, (`Unit, Types.Concrete_ unit_type)
