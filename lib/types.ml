type user_type =
  | Null | Bool | Number | String | Array | List
  | Record | Func

type var =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of var array
  | Symbol of string
  | Identifier of string
  | List of var list
  | Record of (string, var) Hashtbl.t
  | Sepxr of var array
  | UserFunc of {
      args: string array;
      arg_types: user_type array;
      sexpr: var array;
      state: env
    }
  | NativeFunc of {
      args: string array;
      arg_types: user_type array;
      f: env -> var array -> env * var
    }
and env = {outer: env option; map: (string, var) Hashtbl.t}
