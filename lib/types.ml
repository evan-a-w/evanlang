type type_val =
  | Null_
  | Bool_
  | Int_
  | Float_
  | String_
  | Symbol_
  | Identifier_
  | Array_ of typ
  | List_ of typ
  | Map_ of typ * typ
  | Sexpr_
  | Env_
  | SumOrProd of custom_type
  | Func_ of func_type
  | Generic_ of generic
and func_type = {
  func_id: int; (* id used to index into map *)
  args: (string * typ) list; 
  native: bool
}
and typ = type_val * int
and type_env = {
  outer: type_env option; 
  mutable variables: (string, typ) Hashtbl.t;
  mutable functions: (int, func_type) Hashtbl.t;
  mutable traits: (int, trait) Hashtbl.t;
  next_type_id: int ref;
  next_func_id: int ref
}
and sum_member = {tag: string; member_type: typ}
and sum_type = sum_member list
and product_type = {
  fields: (string, typ) Hashtbl.t;
}
and custom_type =
  | Sum of sum_type
  | Product of product_type
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
  | Any


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
  | Custom of {typ: typ; value: custom_instance}
  | Func of func
  | Env of env
and env = {outer: env option; variables: (string, type_instance) Hashtbl.t}
and func = 
  | Native of {
        args: (string * typ) list;
        func_type: typ;
        func: env -> ((string * type_instance) list) -> (env * type_instance)
    }
  | Runtime of {
        args: (string * typ) list;
        func_type: typ;
        env: env;
        sexpr: type_instance array;
    }
and custom_instance = {
  type_id: typ;
  fields: (string, type_instance) Hashtbl.t
}

let ftype = function
  | Native {args = _; func_type = t; func = _} -> t
  | Runtime {args = _; func_type = t; env = _; sexpr = _} -> t

let rec typ_lookup: type_env -> string -> typ option = fun e s ->
  match Hashtbl.find_opt e.variables s with
    | Some v -> Some v
    | None ->
      match e.outer with
        | Some e' -> typ_lookup e' s
        | None -> None

exception UnwrapNone

type parse_error = 
  | TypeError of {expected: typ; found: typ}
  | LitError of {expected: typ; found: string}
  | Malformed of string
  | ParseError of {expected: string; explanation: string}
  | UnknownIdentifier of string

let unwrap x = match x with
  | Some x -> x
  | None -> raise UnwrapNone

let rec type_check_seq:
  type_env -> type_instance Seq.t -> (type_val, parse_error) result 
  = fun e s ->
  let type_seq = Seq.map (type_of e) s in
  match Seq.uncons type_seq with
    | None -> Ok (Generic_ Any)
    | Some (Ok t, rest) ->
      let rec aux (*: (type_val, parse_error) result -> (type_val, parse_error) result *)
        = fun s -> match Seq.uncons s with
        | None -> Ok t
        | Some (Ok t', s') ->
          if t' = t then
            aux s'
          else
            Error (TypeError {expected = t; found = t'})
        | Some (Error e, _) -> Error e in
      aux rest
    | Some (e, _) -> e
and type_of e v = match v with
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
