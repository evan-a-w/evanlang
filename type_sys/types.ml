module TraitSet = Set.Make(String);;
module StringMap = Map.Make(String);;

type typ =
  | Generic_ of TraitSet.t
  | Function_ of typ list
  | Concrete_ of conc_type
and conc_type = {
    type_name : string;
    type_params : typ StringMap.t;
  }
and trait = string
and trait_t = trait_elem list
and trait_elem =
  | Member of { name: string; typ: typ }
  | Type of string
and type_state = {
    trait_impls : (trait * typ) list;
    mutable type's_traits : (typ * TraitSet.t) list;
    type_map : typ StringMap.t;
    trait_map : trait_t StringMap.t;
  }

let id x = x

let sequence_to_string ?(join_by = ", ") ~to_string seq =
  let open Base.Sequence in
  String.concat join_by (map seq ~f:to_string |> to_list)

let seq_to_string ?(join_by = ", ") seq ~to_string =
  String.concat join_by (Seq.map to_string seq |> Stdlib.List.of_seq)

let rec type_to_string = function
  | Function_ typ_list ->
    let typ_seq = Base.Sequence.of_list typ_list in
    sequence_to_string typ_seq ~join_by:" -> " ~to_string:type_to_string
  | Generic_ l ->
    let trait_seq = TraitSet.to_seq l in
    let trait_string = seq_to_string trait_seq ~to_string:id in
    Printf.sprintf "[%s]" trait_string
  | Concrete_ t ->
    let f (s, t2) = Printf.sprintf "%s: %s" s (type_to_string t2) in
    let type_map_string = seq_to_string ~to_string:f (StringMap.to_seq t.type_params) in
    if type_map_string = "" then
      Printf.sprintf "%s" t.type_name
    else
      Printf.sprintf "%s with {%s}" t.type_name type_map_string

let print_type t = Printf.printf "%s\n" (type_to_string t)

let print_impls type_state =
  let f (s, t) = Printf.sprintf "%s for %s" s (type_to_string t) in
  let sequence = Base.Sequence.of_list type_state.trait_impls in
  let seq_string = sequence_to_string sequence ~to_string:f in
  Printf.printf "[%s]\n" seq_string

let print_trait_set ts =
  let tstring = seq_to_string (TraitSet.to_seq ts) ~to_string:id in
  Stdlib.Printf.printf "[%s]\n" tstring

let rec update_assoc_list l k v = match l with
  | [] -> [(k, v)]
  | (xk, xv)::xs -> if xk = k then (k, v) :: xs else (xk, xv) :: update_assoc_list xs k v


let rec get_traits : type_state -> typ -> TraitSet.t = fun ts get_type ->
  let update_traits ts trait_set orig_t (trait, t) =
    (*Printf.printf "[%s] with name %s\nvs\n"
      (seq_to_string (TraitSet.to_seq trait_set) ~to_string:id)
      (Option.value ~default:"None" opt_name);
    print_type t;*)
    (if type_satisfies_super ~a_traits:trait_set ts orig_t t
     then TraitSet.add trait trait_set else trait_set) in
  let rec increment_trait_set : type_state -> TraitSet.t -> typ -> TraitSet.t
    = fun ts tl orig_t ->
    let f trait_set trait_entry = update_traits ts trait_set orig_t trait_entry in
    List.fold_left f tl ts.trait_impls
  and finalise_trait_set : type_state -> TraitSet.t -> typ -> string option -> TraitSet.t
    = fun ts tl orig_t opt_name ->
    let next = increment_trait_set ts tl orig_t in
    if next = tl
    then
      (ts.type's_traits <- update_assoc_list ts.type's_traits orig_t next;
      next)
    else finalise_trait_set ts next orig_t opt_name in
  match List.assoc_opt get_type ts.type's_traits with
  | Some x -> x
  | None -> match get_type with
      | Function_ _ -> TraitSet.empty
      | Generic_ l -> finalise_trait_set ts l get_type None
      | Concrete_ ct -> finalise_trait_set ts TraitSet.empty get_type (Some ct.type_name)
and get_name = function
  | Generic_ _ | Function_ _ -> None
  | Concrete_ { type_name; _ } -> Some type_name
and type_satisfies_super ?a_traits ts a b = 
  a = b ||
  let ta = match a_traits with None -> get_traits ts a | Some x -> x in
  match a, b with
    | (Function_ la, Function_ lb) -> la = lb
    | (Function_ _, _) -> false
    | (_, Generic_ tb) -> TraitSet.subset tb ta
    | (Generic_ _, _) -> false
    | (Concrete_ ca, Concrete_ cb) ->
      ca.type_name = cb.type_name &&
      StringMap.equal (type_satisfies_super ts) ca.type_params cb.type_params
    | (Concrete_ _, Function_ _) -> false

let add_type : type_state -> (string * typ) -> type_state = fun ts (name, t) ->
  let inserted = match t with
    | Concrete_ x -> Concrete_ { x with type_name = name }
    | o -> o in
  { ts with type_map = StringMap.add name inserted ts.type_map }

let add_trait : type_state -> (trait * trait_t) -> type_state
  = fun ts (name, els) ->
  { ts with trait_map = StringMap.add name els ts.trait_map;
            type's_traits = [] }

let add_impl : type_state -> (trait * typ) -> type_state
  = fun ts (name, t) ->
  { ts with trait_impls = (name, t) :: ts.trait_impls;
            type's_traits = [] }
