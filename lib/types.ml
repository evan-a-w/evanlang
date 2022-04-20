type typ =
  | Null_
  | Bool_
  | Int_
  | Float_
  | String_
  | Symbol_
  | Identifier_
  | Array_ of type_id
  | List_ of type_id
  | Map_ of type_id * type_id
  | Sexpr_
  | Env_
  | SumOrProd of custom_type
  | Custom_ of type_id (* refers to SumOrProd usually - could also be a unique base type ig *)
  | Func_ of func_type
  | Generic_ of generic
and func_type = {
  func_id: int; (* id used to index into map *)
  args: (string * type_id) list; 
  native: bool
}
and type_env = {
  outer: type_env option; 
  mutable variables: (string, type_id) Hashtbl.t;
  mutable functions: (int, func_type) Hashtbl.t;
  mutable types: (type_id, typ) Hashtbl.t;
  mutable traits: (type_id, trait) Hashtbl.t;
  mutable next_type_id: int;
  mutable next_func_id: int
}
and sum_member = {tag: string; member_type: type_id}
and sum_type = sum_member list
and product_type = {
  fields: (string, type_id) Hashtbl.t;
}
and custom_type =
  | Sum of sum_type
  | Product of product_type
and trait = string * trait_elem
and trait_elem =
  | Trait of trait
  | Field of {
      name: string;
      typ: type_id
    }
  | Method of {
      name: string;
      fn: func_type
    }
and generic =
  | Traits of trait list
  | Val of type_id
and type_id = string


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
  | Sexpr of type_instance list
  | Custom of {type_id: type_id; value: custom_instance}
  | Func of func
  | Env of env
and env = {outer: env option; variables: (string, type_instance) Hashtbl.t}
and func = 
  | Native of {
        args: (string * type_id) list;
        func_type: type_id;
        func: env -> ((string * type_instance) list) -> (env * type_instance)
    }
  | Runtime of {
        args: (string * type_id) list;
        func_type: type_id;
        env: env;
        sexpr: type_instance array;
    }
and custom_instance = {
    type_id: type_id;
    fields: (string, type_instance) Hashtbl.t
}

let ftype = function
  | Native {args = al; func_type = i; func = _} -> 
      {func_id = i; args = al; native = true}
  | Runtime {args = al; func_type = i; env = _; sexpr = _} ->
      {func_id = i; args = al; native = false}

let rec type_id_lookup: type_env -> string -> type_id option = fun e s ->
  match Hashtbl.find_opt e.variables s with
    | Some v -> Some v
    | None ->
      match e.outer with
        | Some e' -> type_id_lookup e' s
        | None -> None

exception UnwrapNone

let unwrap x = match x with
  | Some x -> x
  | None -> raise UnwrapNone

let rec type_from_id e i =
  match Hashtbl.find_opt e.types i with
    | Some t -> t
    | None -> type_from_id (unwrap e.outer) i

let type_lookup e s = 
  match type_id_lookup e s with
    | Some i -> Some (type_from_id e i)
    | None -> None

let type_check_seq = ()

let type_of e v = match v with
  | Null -> Ok Null_
  | Bool _ -> Ok Bool_
  | Int _ -> Ok Int_
  | Float _ -> Ok Float_
  | String _ -> Ok String_
  | Array _ -> Ok Array_
  | Symbol _ -> Ok Symbol_
  | Identifier s -> type_lookup e s |> unwrap |> Result.ok
  | List _ -> List_
  | Map _ -> Map_
  | Sexpr _ -> Sexpr_
  | Env _ -> Env_
  | Custom {type_id = t; value = _} -> Custom_ t
  | Func f -> Func_ (ftype f)
