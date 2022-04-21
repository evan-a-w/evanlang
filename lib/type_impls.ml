open Types

let initial_type_env =
  let initial_type_map = Hashtbl.of_seq (List.to_seq [
      ( 0, Null_);
      ( 1, Bool_);
      ( 2, Int_);
      ( 3, Float_);
      ( 4, String_);
      ( 5, Array_);
      ( 6, Symbol_);
      ( 7, Identifier_);
      ( 8, List_);
      ( 9, Map_);
      (10, Sepxr_);
      (11, Env_);
    ]) in
  let initial_func_map = Hashtbl.create 10 in
  let initial_trait_map = Hashtbl.create 10 in
  {
    outer = None;
    variables = Hashtbl.create 10;
    functions = initial_func_map;
    types = initial_type_map;
    traits = initial_trait_map;
    next_type_id = Hashtbl.length initial_type_map;
    next_func_id = Hashtbl.length initial_func_map;
  }