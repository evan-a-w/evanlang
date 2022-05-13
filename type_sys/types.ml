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

let rec get_traits : type_state -> typ -> TraitSet.t = fun ts t ->
  let update_traits ts trait_set opt_name (trait, t) =
    let nt = get_name t in
    (if (Option.is_none nt || nt = opt_name)
        && type_subset_info'd ts trait_set opt_name t
     then TraitSet.add trait trait_set else trait_set) in
  let rec increment_trait_set : type_state -> TraitSet.t -> string option -> TraitSet.t
    = fun ts tl opt_name ->
    let f trait_set trait_entry = update_traits ts trait_set opt_name trait_entry in
    List.fold_left f tl ts.trait_impls
  and finalise_trait_set : type_state -> TraitSet.t -> string option -> TraitSet.t
    = fun ts tl opt_name ->
    let next = increment_trait_set ts tl opt_name in
    if next = tl
    then
      (Option.map
         (fun n -> ts.type's_traits <- StringMap.add n next ts.type's_traits)
         opt_name
       |> void;
      next)
    else finalise_trait_set ts next opt_name in
  match t with
  | Function_ _ -> TraitSet.empty
  | Generic_ l -> finalise_trait_set ts l None
  | Concrete_ ct ->
    let init_set = Option.value
                     ~default:(TraitSet.empty)
                     (StringMap.find_opt ct.type_name ts.type's_traits) in
    finalise_trait_set ts init_set (Some ct.type_name)
and get_name = function
  | Generic_ _ | Function_ _ -> None
  | Concrete_ { type_name; type_params = _ } -> Some type_name
and type_subset_info'd ts trait_set opt_name b =
  let bs = get_traits ts b in
  get_name b = opt_name && TraitSet.subset trait_set bs
and type_subset ts a b = 
  a = b ||
    let ta = get_traits ts a and na = get_name a in
    type_subset_info'd ts ta na b

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
