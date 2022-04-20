type typ =
  | Null
  | Bool
  | Int
  | Float
  | String
  | Array
  | Symbol
  | Identifier
  | List
  | Map
  | Sepxr
  | Env
  | Custom of custom_type
  | Func of func_type
  | Generic of generic
and func_type = {
  func_id: int; (* id used to index into map *)
  args: (string * typ) list; 
  native: bool
}
and type_env = {
  outer: type_env option; 
  mutable variables: (string, typ) Hashtbl.t;
  mutable functions: (int, func_type) Hashtbl.t;
  mutable types: (int, (typ * (trait list))) Hashtbl.t;
  mutable next_type_id: int;
  mutable next_func_id: int
}
and sum_member = {tag: string; member_type: typ}
and sum_type = sum_member list
and product_type = {
  fields: (string, typ) Hashtbl.t;
}
and sum_or_prod =
  | Sum of sum_type
  | Product of product_type
and custom_type = {typ: sum_or_prod; typ_id: int}
and trait = string * trait_elem
and trait_elem =
  | Trait of trait
  | Field of {
      name: string;
      typ: typ
    }
  | Method of {
      name: string;
      fn: func_type
    }
and generic =
  | Traits of trait list
  | Val of typ

type type_id = int


(* For the interpreter, type checking should be done by the time this is used
 * so runtime type errors should never occur *)
type type_instance =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of type_instance array
  | Symbol of string
  | Identifier of string
  | List of type_instance list
  | Map of (string, type_instance) Hashtbl.t
  | Sepxr of type_instance array
  | Custom of {type_id: type_id; value: custom_instance}
  | Func of func
  | Env of env
and env = {outer: env option; variables: (string, type_instance) Hashtbl.t}
and func = 
  | Native of {
        args: (string * type_id) list;
        func: env -> ((string * type_instance) list) -> (env * type_instance)
    }
  | Runtime of {
        args: (string * type_id) list;
        env: env;
        sexpr: type_instance array;
    }
and custom_instance = {
    type_id: type_id;
    fields: (string, type_instance) Hashtbl.t
}
