module StringSet = Set.Make(String)

type poly_type =
  | Generic_ of generic_type
  | Concrete_ of concrete_type
and trait = string * (trait_element list)
and trait_element = 
  | Type_parameter of concrete_type
  | Member of {
      field_name: string;
      field_type: concrete_type;
    }
and generic_type = StringSet.t
and concrete_type =
  | Unit_
  | Bool_
  | Int_
  | String_
  | Double_
  | Array_ of poly_type
  | List_ of poly_type
  | Map_ of poly_type * poly_type
  | Tuple_ of poly_type list
  | Func_ of poly_type list
  | Custom_ of sum_or_prod
and sum_or_prod =
  | Sum of (string, poly_type) Hashtbl.t
  | Product of (string, poly_type) Hashtbl.t
and trait_map = (string, trait) Hashtbl.t
and trait_members = (string, poly_type list) Hashtbl.t
and concrete_trait_set = (concrete_type * StringSet.t) list
and trait_struct = {
  mutable map: trait_map;
  mutable members: trait_members;
  mutable concrete_set: concrete_trait_set;
  (* For every trait, there is a list of list of traits that are implemented automatically *)
  mutable auto_impls: (string * (StringSet.t list)) list;
  (* Will also have type -> conditions on member types -> traits *)
  (* Probably implemented as associative list with type, check equivalence and then get the auot doobs *)
}

let get_traits_of_trait_set trait_st trait_set =
  let satisfy_trait_set tset = StringSet.subset tset trait_set in
  let satisfies (s, tsl) = if List.exists satisfy_trait_set tsl then Some s else None in
  let auto_impl_satisfied = List.filter_map satisfies trait_st.auto_impls in
  StringSet.union (StringSet.of_seq (List.to_seq auto_impl_satisfied)) trait_set

(* All the traits implemented directly by a type as well as by the traits implemented by the type and so on *)
let get_traits_of_concrete trait_st conc =
  let first = List.assoc conc trait_st.concrete_set in
  get_traits_of_trait_set trait_st first

let get_traits_of_poly trait_st = function
  | Concrete_ t -> get_traits_of_concrete trait_st t
  | Generic_ g -> get_traits_of_trait_set trait_st g

let concrete_satisfies_generic trait_st conc gen = 
  let concrete_traits = get_traits_of_concrete trait_st conc in
  let generic_traits = get_traits_of_trait_set trait_st gen in
  StringSet.subset concrete_traits generic_traits

let satisfies_traits trait_st poly trait_set =
  StringSet.subset trait_set (get_traits_of_poly trait_st poly)

type type_error =
  | MisMatch of poly_type * poly_type

let reduce_types trait_st a b = match (a, b) with
  | (Concrete_ ac, Concrete_ bc) -> if ac = bc then Ok a else Error (MisMatch (a, b))
  | (Concrete_ ac, Generic_ bc) -> if concrete_satisfies_generic trait_st ac bc
                                      then Ok a
                                      else Error (MisMatch (a, b))
  | (Generic_ ac, Concrete_ bc) -> if concrete_satisfies_generic trait_st bc ac
                                      then Ok b
                                      else Error (MisMatch (a, b))
  | (Generic_ ac, Generic_ bc) ->
    let ac_traits = get_traits_of_trait_set trait_st ac
    and bc_traits = get_traits_of_trait_set trait_st bc in
    if StringSet.subset ac_traits bc_traits
      then Ok a
    else if StringSet.subset bc_traits ac_traits
      then Ok b
    else Error (MisMatch (a, b))
