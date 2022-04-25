open Types

(* Used to refer to own type in generic  *)
let self = ("Self", [])

let hash = ("Hash", [Method {name = "hash"; typ = [Generic_ [self], Int_]}])

let void_func = ([], [any; Unit_], true)

let initial_traits = Hashtbl.of_seq (List.to_seq [self; hash])

let initial_trait_map =
  Hashtbl.of_seq (Seq.zip (Seq.ints 0) (Hashtbl.to_seq_values initial_traits))

let initial_type_map =
  Hashtbl.of_seq 
    (Seq.zip
       (Seq.ints 0)
       (List.to_seq [
          Unit_; Bool_; Int_; Float_; String_; Array_ any; Symbol_; List_ any;
          Map_ (Generic_ [hash], any);
        ]))

let initial_functions = Hashtbl.of_seq (List.to_seq [])

let initial_func_map =
  Hashtbl.of_seq (Seq.zip (Seq.ints 0) (Hashtbl.to_seq_values initial_functions))

let initial_type_env =
  {
    outer = None;
    variables = initial_functions;
    functions = initial_func_map;
    traits = initial_traits;
    trait_map = initial_trait_map;
    next_type_id = 0;
    next_func_id = Hashtbl.length initial_func_map;
  }
