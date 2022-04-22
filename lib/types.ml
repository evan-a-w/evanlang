open Result

type typ =
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
  | Custom_ of custom_type
  | Func_ of func_type
  | Tuple_ of typ list
  | Generic_ of trait list
and func_type = {
  func_id: int; (* id used to index into map *)
  args: (string * typ) list; 
  native: bool
}
and type_env = {
  outer: type_env option; 
  mutable variables: (string, typ) Hashtbl.t;
  mutable functions: (int, func_type) Hashtbl.t;
  mutable traits: (string, trait) Hashtbl.t;
  mutable trait_map: (int, string list) Hashtbl.t;
  next_type_id: int ref;
  next_func_id: int ref
}
and sum_member = {tag: string; member_type: typ}
and sum_or_prod =
    | Sum of (string, typ) Hashtbl.t
    | Prod of (string, typ) Hashtbl.t
and custom_type = sum_or_prod * int
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
  | Map of typ * (string, type_instance) Hashtbl.t
  | Sexpr of type_instance list
  | Custom of typ * (string, type_instance) Hashtbl.t
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
exception NotTraitable of typ

type parse_error = 
  | TypeError of {expected: typ; found: typ}
  | LitError of {expected: typ; found: string}
  | Malformed of string
  | ParseError of {expected: string; explanation: string}
  | UnknownIdentifier of string
  | AbstractTypeError of {expected: string; found: typ}
  | FlippedATE of {found: string; expected: typ}

let get_custom = function
    | Custom_ (t, id) -> Ok (t, id)
    | t -> Error (AbstractTypeError {expected = "custom"; found = t})

let unwrap x = match x with
  | Some x -> x
  | None -> raise UnwrapNone

let func_type_list = function
    | Func_ {args = a; func_id = _; native = _} -> List.map (fun (s, t) -> t) a |> ok
    | t -> Error (AbstractTypeError {expected = "Function"; found = t})

let all_but_last l =
  let rec inner acc = function
    | [] -> raise UnwrapNone
    | [x] -> ((List.rev acc), x)
    | x :: xs -> inner (x :: acc) xs in
  match l with
    | [] -> Error (Malformed "Function defined with no type")
    | o -> Ok (inner [] o)

let get_type_id = function
  | Custom_ (_, id) -> id
  | Null_ -> 0
  | Bool_ -> 1
  | Int_ -> 2
  | Float_ -> 3
  | String_ -> 4
  | Array_ _ -> 5
  | Symbol_ -> 6
  | List_ _ -> 7
  | Map_ _ -> 8
  | t -> raise (NotTraitable t)

let get_trait_list e t = match Hashtbl.find_opt e.trait_map (get_type_id t) with
  | Some es -> es
  | None -> []

let rec lift_opt = function
  | [] -> Ok []
  | x :: xs ->
    bind x (fun x' ->
    let lifted = lift_opt xs in
    Result.map (fun y -> x' :: y) lifted)

let rec type_check_seq:
  type_env -> type_instance Seq.t -> (typ, parse_error) result 
  = fun e s ->
  let type_seq = Seq.map (type_of e) s in
  match Seq.uncons type_seq with
    | None -> Ok (Generic_ [])
    | Some (Ok t, rest) ->
      let rec aux (*: (typ, parse_error) result -> (typ, parse_error) result *)
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
  | Symbol _ -> Ok Symbol_
  | Identifier s -> Option.to_result (typ_lookup e s) ~none:(UnknownIdentifier s)
  | Array a -> bind (type_check_seq e (Array.to_seq a)) (fun t -> Array_ t |> ok)
  | List l -> bind (type_check_seq e (List.to_seq l)) (fun t -> List_ t |> ok)
  | Map (k, m) -> 
    bind (type_check_seq e (Hashtbl.to_seq_values m)) (fun v ->
    Ok (Map_ (k, v)))
  | Sexpr l -> (match l with
    | [] -> Ok Null_
    | x :: xs -> 
      bind (type_of e x) (fun t -> 
      bind (func_type_list t) (fun ft ->
      bind (lift_opt (List.map (fun z -> type_of e z) xs)) (fun ts ->
      bind (all_but_last ft) (fun (first, last) ->
      if ts = first then
        Ok last
      else
        Error (TypeError {expected = Tuple_ first; found = Tuple_ ts}))))))
  | Env _ -> Ok Env_
  | Custom (t, v) ->
    bind (get_custom t) (fun (t', id) ->
    match t' with
    | Sum st -> (match List.of_seq (Hashtbl.to_seq v) with
      | [(k, v)] ->
        bind (Hashtbl.find_opt st k |>
              Option.to_result
                (Error (FlippedAte {
                  expected = t';
                  found = Printf.sprintf "Sum option %s" k
        }))) (fun mt ->
        bind (typeof e v) (fun vt ->
        if vt = t' then
          Ok (Custom_ (t', id))
        else
          Error (TypeError {expected = t'; found = vt})))
      | _ -> Error (FlippedATE {expected = t; found = "Record struct"}))
    | Prod pt ->
      bind (lift_opt (Seq.map (typeof e) (Hashtbl.to_seq_values v))) (fun ms ->
      let mapped_v = Seq.zip (Seq.to_seq_keys v) ms in
      let sm = Seq.sorted_merge String.compare mapped_v (Hashtbl.to_seq pt) in
      )
  | Func f -> Func_ (ftype f)
