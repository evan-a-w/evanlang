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
    mutable type's_traits : TraitSet.t StringMap.t;
    type_map : typ StringMap.t;
    trait_map : trait_t StringMap.t;
  }

let void _ = ()

let rec get_traits : type_state -> typ -> TraitSet.t = fun ts get_type ->
  let update_traits ts trait_set orig_t opt_name (trait, t) =
    (if t = orig_t || match t with
          | Generic_ bs -> TraitSet.subset bs trait_set
          | Function_ _ -> t = orig_t
          | Concrete_ _ -> type_superset_with_info ts trait_set opt_name t
     then TraitSet.add trait trait_set else trait_set) in
  let rec increment_trait_set : type_state -> TraitSet.t -> typ -> string option -> TraitSet.t
    = fun ts tl orig_t opt_name ->
    let f trait_set trait_entry = update_traits ts trait_set orig_t opt_name trait_entry in
    List.fold_left f tl ts.trait_impls
  and finalise_trait_set : type_state -> TraitSet.t -> typ -> string option -> TraitSet.t
    = fun ts tl orig_t opt_name ->
    let next = increment_trait_set ts tl orig_t opt_name in
    if next = tl
    then
      (Option.map
         (fun n -> ts.type's_traits <- StringMap.add n next ts.type's_traits)
         opt_name
       |> void;
      next)
    else finalise_trait_set ts next orig_t opt_name in
  match get_type with
  | Function_ _ -> TraitSet.empty
  | Generic_ l -> finalise_trait_set ts l get_type None
  | Concrete_ ct ->
    let init_set = get_traits_weak ts get_type in
    finalise_trait_set ts init_set get_type (Some ct.type_name)
and get_traits_weak ts t = match t with
  | Generic_ l -> l
  | Function_ _ -> TraitSet.empty
  | Concrete_ ct -> Option.value
                      ~default:(TraitSet.empty)
                      (StringMap.find_opt ct.type_name ts.type's_traits)
and get_name = function
  | Generic_ _ | Function_ _ -> None
  | Concrete_ { type_name; _ } -> Some type_name
and type_superset_with_info ts trait_set opt_name b =
  let bname = get_name b in
  (Option.is_none bname || bname = opt_name) &&
    let bs = get_traits ts b in
    TraitSet.subset bs trait_set
and type_subset ts a b = 
  a = b ||
    let tb = get_traits ts b and nb = get_name b in
    type_superset_with_info ts tb nb a

let add_type : type_state -> (string * typ) -> type_state = fun ts (name, t) ->
  let inserted = match t with
    | Concrete_ x -> Concrete_ { x with type_name = name }
    | o -> o in
  { ts with type_map = StringMap.add name inserted ts.type_map }

let add_trait : type_state -> (trait * trait_t) -> type_state
  = fun ts (name, els) ->
  { ts with trait_map = StringMap.add name els ts.trait_map }

let add_impl : type_state -> (trait * typ) -> type_state
  = fun ts (name, t) ->
  { ts with trait_impls = (name, t) :: ts.trait_impls }
