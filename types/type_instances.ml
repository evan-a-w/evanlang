open Types

type type_state = {
  mutable custom_types: (string, sum_or_prod) Hashtbl.t;
  mutable env: env;
  mutable trait_st: trait_struct
}
and env = {
  outer: env option;
  var_vals: (string, type_instance) Hashtbl.t;
}
and type_instance =
  | Unit
  | Bool of bool
  | Int of int
  | String of string
  | Double of float
  | Array of type_instance array
  | List of type_instance list
  | Map of (string, type_instance) Hashtbl.t
  | Tuple of type_instance list
  | Func of func_instance
  | Custom of custom_instance
  | Generic of generic_type
and func_instance = {
  func_type: poly_type list;
}
and custom_instance = {
  custom_type: sum_or_prod;
  fields: (string, type_instance) Hashtbl.t
}
