open Types

let type_check trait_st t = match t with
  | Generic_ _ -> Ok t
  | Concrete_ conc -> match conc with
    | Unit_
    | Bool_
    | Int_
    | String_
    | Double_
    | Array_ p
    | List_ of poly_type
    | Map_ of poly_type * poly_type
    | Tuple_ of poly_type list
    | Func_ of poly_type list
    | Custom_ of sum_or_prod
