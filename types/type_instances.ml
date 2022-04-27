type type_instance =
  | Unit
  | Bool
  | Int
  | String
  | Double
  | Array of type_instance array
  | List of type_instance list
  | Map of (type_instance, type_instance) Hashtbl.t
  | Tuple of type_instance list
  | Func of func_instance
  | Custom of custom_instance
