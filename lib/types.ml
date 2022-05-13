open Result

type typ =
  | Unit_
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
  args: string list; 
  typ: typ list;
  native: bool
}
and type_env = {
  outer: type_env option; 
  mutable type_map: (int, typ) Hashtbl.t;
  mutable variables: (string, typ) Hashtbl.t;
  mutable functions: (int, func_type) Hashtbl.t;
  mutable traits: (string, trait) Hashtbl.t;
  mutable traits_of_type_id: (int, (string, bool) Hashset.t) Hashtbl.t;
  next_type_id: int ref;
  next_func_id: int ref
}
and sum_member = {tag: string; member_type: typ}
and sum_or_prod =
    | Sum of (string, typ) Hashtbl.t
    | Prod of (string, typ) Hashtbl.t
and custom_type = sum_or_prod * int
and trait = string * (trait_elem list)
and trait_elem =
  | Trait of trait
  | Member of {
      name: string;
      typ: typ
    }
  | Method of {
      name: string;
      typ: typ list;
    }

let any_type = Generic_ []

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

let rec typ_lookup_res: type_env -> string -> (typ, parse_error) result = fun e s ->
  match Hashtbl.find_opt e.variables s with
    | Some v -> Ok v
    | None ->
      match e.outer with
        | Some e' -> typ_lookup_res e' s
        | None -> Error (UnknownIdentifier s)

let ftype f = let get = function
  | Native {args = _; func_type = t; func = _} -> t
  | Runtime {args = _; func_type = t; env = _; sexpr = _} -> t in
  match get f with
    | Func_ t -> Ok (Func_ t)
    | o -> Error (AbstractTypeError {expected = "Function"; found = o})

let get_custom = function
    | Custom_ (t, id) -> Ok (t, id)
    | t -> Error (AbstractTypeError {expected = "custom"; found = t})

let unwrap x = match x with
  | Some x -> x
  | None -> raise UnwrapNone

let all_but_last l =
  let rec inner acc = function
    | [] -> raise UnwrapNone
    | [x] -> ((List.rev acc), x)
    | x :: xs -> inner (x :: acc) xs in
  match l with
    | [] -> Error (Malformed "Function defined with no type")
    | o -> Ok (inner [] o)

let func_type_list = function
    | Func_ {args = _; func_id = _; typ = t; native = _} -> all_but_last t
    | t -> Error (AbstractTypeError {expected = "Function"; found = t})

let get_type_id = function
  | Custom_ (_, id) -> id
  | Unit_ -> 0
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

let get_type_id = function
  | Custom_ (_, id) -> id
  | Unit_ -> 0
  | Bool_ -> 1
  | Int_ -> 2
  | Float_ -> 3
  | String_ -> 4
  | Array_ _ -> 5
  | Symbol_ -> 6
  | List_ _ -> 7
  | Map_ _ -> 8
  | t -> raise (NotTraitable t)

let satisfies e t trait_list =
  let satisfies_trait (s, _) =
    Option.is_some (Option.map
                     (fun m -> Hashtbl.find_opt m s)
                     (Hashtbl.find_opt e.trait_map (get_type_id s))) in
  List.for_all satisfies_trait trait_list

let get_gen_trait e t = match t with
  | Bool_
  | Int_
  | Float_
  | String_
  | Symbol_
  | Sexpr_
  | Env_
  | Func_ _
  | Custom_ _
  | Func_ _
  | Tuple_ _ -> Ok []
  | _ -> raise (NotTraitable t)

let concrete e a = match a with
  | Unit_
  | Bool_
  | Int_
  | Float_
  | String_
  | Symbol_
  | Sexpr_
  | Env_
  | Func_ _
  | Custom_ _
  | Func_ _
  | Tuple_ _ -> Ok true
  | Array_ b -> concrete e b
  | List_ b -> concrete e b
  | Map_ b c -> (concrete e b) && (concrete e c)
  | Identifier s = Result.map (concrete e) (typ_lookup e s)
  | _ -> Ok false


let most_specific_type e a b =
  bind (concrete e a) (fun a_concrete ->
    if a_concrete then
      a
    else
      bind (concrete e b) (fun b_concrete ->
        if b_concrete then
          if 
        else
          let a_traits = get_gen_trait e a in

let typ_compatible e a b = 
  match a with
  | Unit_
  | Bool_
  | Int_
  | Float_
  | String_
  | Symbol_
  | Sexpr_
  | Env_
  | Func_ _
  | Custom_ _
  | Tuple_ _ -> if a = b then Ok a
  | Array_ at -> 
  | List_ of typ
  | Map_ of typ * typ
  | Custom_ of custom_type
  | Generic_ of trait list

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
  | Null -> Ok Unit_
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
    | [] -> Ok Unit_
    | x :: xs -> 
      bind (type_of e x) (fun t -> 
      bind (func_type_list t) (fun (first, last) ->
      bind (lift_opt (List.map (fun z -> type_of e z) xs)) (fun ts ->
      if ts %= first then
        Ok last
      else
        Error (TypeError {expected = Tuple_ first; found = Tuple_ ts})))))
  | Env _ -> Ok Env_
  | Custom (t, v) ->
    bind (get_custom t) (fun (t', id) ->
    match t' with
    | Sum st -> (match List.of_seq (Hashtbl.to_seq v) with
      | [(k, v)] ->
        bind (Option.to_result ~none:(FlippedATE {
                  expected = Custom_ (t', id);
                  found = Printf.sprintf "Sum option %s" k
        }) (Hashtbl.find_opt st k)) (fun mt ->
        bind (type_of e v) (fun vt ->
        if vt %= mt then
          Ok (Custom_ (t', id))
        else
          Error (TypeError {expected = Custom_ (t', id); found = vt})))
      | _ -> Error (FlippedATE {expected = t; found = "Record struct"}))
    | Prod pt ->
      bind (Result.map List.to_seq
                       (lift_opt
                         (List.of_seq
                            (Seq.map (type_of e) (Hashtbl.to_seq_values v)))))
      (fun ms ->
      let mapped_v = Seq.zip (Hashtbl.to_seq_keys v) ms in
      let sort_thing thing = 
        List.sort
          (fun (s, _) (s', _ )-> String.compare s s')
          (List.of_seq thing) in
      let sorted_v = List.to_seq (sort_thing mapped_v) in
      let sorted_pt = List.to_seq (sort_thing (Hashtbl.to_seq pt)) in
      let two_eq ((s, t), (s', t')) = s = s' and t %= t' in
      let joined = Seq.zip sorted_v sorted_pt in
      if Seq.for_all two_eq joined then
        Ok (Custom_ (t', id))
      else
        Error (FlippedATE {expected = Custom_ (t', id); found = "Wrong user struct"})))
  | Func f -> ftype f
