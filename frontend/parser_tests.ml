open Angstrom
open Parser
open Core

let%test "int_succ" =
  let parsed = parse_string ~consume:All int_p "1" in
  match parsed with
  | Error e -> (
      fprintf stderr "%s" e;
      false
    )
  | Ok i ->
    if i = 1 then true else (
      fprintf stderr "%d" i;
      false
    )

let%test "float_succ" =
  let parsed = parse_string ~consume:All float_p "1.5" in
  match parsed with
  | Error e -> (
      fprintf stderr "%s" e;
      false
    )
  | Ok f ->
    if Float.equal f 1.5  then true else (
      fprintf stderr "%f" f;
      false
    )

let%test "string_succ" =
  let parsed = parse_string ~consume:All string_p "\"hello\\\"\"" in
  match parsed with
  | Ok "hello\"" -> true
  | _ -> false

let%test "string_exp" =
  let parsed = parse_string ~consume:All exp_p "\"hello\\\"\"" in
  match parsed with
  | Ok (`StringLit "hello\"") -> true
  | _ -> false

let%test "trait_list_succ" =
  let parsed =
    parse_string ~consume:Prefix trait_list_p
                 " [Ord, Show] " in
  match parsed with
  | Ok ["Ord"; "Show"] -> true
  | _ -> false

let%test "trait_spec_succ" =
  let parsed =
    parse_string ~consume:Prefix trait_spec_p
                 " (where (a is [Eq]) (b is [Ord, Show])) " in
  match parsed with
  | Ok ["a", ["Eq"]; "b", ["Ord"; "Show"]] -> true
  | _ -> false

let%test "type_expr_succ" =
  let parsed =
    parse_string ~consume:Prefix type_expr_p
                 "ning lol x" in
  match parsed with
  | Ok (`Single (["ning"; "lol"], "x")) -> true
  | _ -> false

let%test "reserved_id" = 
  let parsed =
    parse_string ~consume:Prefix identifier_p
                 "->" in
  match parsed with
  | Error _ -> true
  | _ -> false

let%test "type_expr_multi" =
  let parsed : (Types.type_expr, string) result =
    parse_string ~consume:Prefix type_expr_p
                 "ning lol x -> bing bong tex -> zeit" in
  match parsed with
  | Ok (`Multi
    [ `Single (["ning"; "lol"], "x")
    ; `Single (["bing"; "bong"], "tex")
    ; `Single ([], "zeit")
    ]) -> true
  | _ -> false

let%test "type_expr_multi_fun" =
  let parsed : (Types.type_expr, string) result =
    parse_string ~consume:Prefix type_expr_p
                 "ning lol x -> (bing bong tex -> zeit)" in
  match parsed with
  | Ok (`Multi
    [ `Single (["ning"; "lol"], "x")
    ; `Multi [`Single ((["bing"; "bong"]), "tex"); `Single ([], "zeit")]
    ]) -> true
  | _ -> false

let%test "sum_type_succ" =
  let parsed =
    parse_string ~consume:Prefix sum_type_p
                 " (Some of x, None ) " in
  match parsed with
  | Ok [("Some", Some (`Single ([], "x"))); ("None", None)] -> true
  | _ -> false

let%test "prod_type_succ" =
  let parsed =
    parse_string ~consume:Prefix prod_type_p
                 " {Some of x, None of a b c } " in
  match parsed with
  | Ok [("Some", `Single ([], "x")); ("None", `Single (["a"; "b"], "c"))] -> true
  | _ -> false

let%test "deftype_sum_all" =
  let parsed =
    parse_string ~consume:Prefix exp_p
    "( deftype (a b c)
               (where (a is [Eq]) (b is [Ord, Show]))
        (Some of x, None )
     )" in
  let texpr = `Single (["a"; "b"], "c") in
  let whereexpr = ["a", ["Eq"]; "b", ["Ord"; "Show"]] in
  let sumexpr = [("Some", Some (`Single ([], "x"))); ("None", None)] in
  let open Stdlib in
  match parsed with
  | Ok (`DefType (a, b, c)) when (a = texpr && b = whereexpr && c = `Sum sumexpr) -> true
  | _ -> false

let%test "deftype_sum_not_all" =
  let parsed =
    parse_string ~consume:Prefix exp_p
    "( deftype (a b c)
        (Some of x, None )
     )" in
  let texpr = `Single (["a"; "b"], "c") in
  let whereexpr = [] in
  let sumexpr = [("Some", Some (`Single ([], "x"))); ("None", None)] in
  let open Stdlib in
  match parsed with
  | Ok (`DefType (a, b, c)) when (a = texpr && b = whereexpr && c = `Sum sumexpr) -> true
  | _ -> false

let%test "deftype_prod_not_all" =
  let parsed =
    parse_string ~consume:Prefix exp_p
    "( deftype (a b c)
        {Some of x, None of a b s}
     )" in
  let texpr = `Single (["a"; "b"], "c") in
  let whereexpr = [] in
  let prodexpr = [("Some", `Single ([], "x")); ("None", `Single (["a"; "b"], "s"))] in
  let open Stdlib in
  match parsed with
  | Ok (`DefType (a, b, c)) when (a = texpr && b = whereexpr && c = `Prod prodexpr) -> true
  | _ -> false

let%test "deftype_prod_all" =
  let parsed =
    parse_string ~consume:Prefix exp_p
    "( deftype (a b c)
               (where (a is [Eq]) (b is [Ord, Show]))
        {Some of x, None of a b s}
     )" in
  let texpr = `Single (["a"; "b"], "c") in
  let whereexpr = ["a", ["Eq"]; "b", ["Ord"; "Show"]] in
  let prodexpr = [("Some", `Single ([], "x")); ("None", `Single (["a"; "b"], "s"))] in
  let open Stdlib in
  match parsed with
  | Ok (`DefType (a, b, c)) when (a = texpr && b = whereexpr && c = `Prod prodexpr) -> true
  | _ -> false

let%test "id_+" = let open Stdlib in
  (parse_string ~consume:Prefix exp_p "+") = (Ok (`Identifier "+"))

let%test "call_p" =
  let parsed =
    parse_string ~consume:Prefix exp_p "(+ 1 2)" in
  match parsed with
  | Ok (`Call (`Identifier "+", [`IntLit 1; `IntLit 2])) -> true
  | _ -> false

let%test "list_p" =
  let parsed =
    parse_string ~consume:Prefix exp_p "'(+ 1 2)" in
  match parsed with
  | Ok (`ListLit [`Identifier "+"; `IntLit 1; `IntLit 2]) -> true
  | _ -> false

let%test "array_p" =
  let parsed =
    parse_string ~consume:Prefix exp_p "[+ 1 2]" in
  match parsed with
  | Ok (`ArrayLit [|`Identifier "+"; `IntLit 1; `IntLit 2|]) -> true
  | _ -> false

let%test "list_p_inner" = let open Stdlib in
  let parsed =
    parse_string ~consume:Prefix exp_p "'(1 \"hi\" '(+ 1 2))" in
  let first_list = `ListLit [`Identifier "+"; `IntLit 1; `IntLit 2] in
  match parsed with
  | Ok (`ListLit [`IntLit 1; `StringLit "hi"; l]) when l = first_list -> true
  | _ -> false

let%test "typed_p" = let open Stdlib in
  let parsed =
    parse_string ~consume:Prefix exp_p "(typed (a b) '(+ 1 2))" in
  let lis = [`Identifier "+"; `IntLit 1; `IntLit 2] in
  let typ = `Single (["a"], "b") in
  match parsed with
  | Ok (`Typed (t, `ListLit l)) when t = typ && l = lis -> true
  | _ -> false

let%test "deftype_sum_all_prog" =
  let parsed =
    parse_string ~consume:All prog_p
    "(deftype (a b c)
               (where (a is [Eq]) (b is [Ord, Show]))
        (Some of x, None ))" in
  let texpr = `Single (["a"; "b"], "c") in
  let whereexpr = ["a", ["Eq"]; "b", ["Ord"; "Show"]] in
  let sumexpr = [("Some", Some (`Single ([], "x"))); ("None", None)] in
  let open Stdlib in
  match parsed with
  | Ok [(`DefType (a, b, c))] when (a = texpr && b = whereexpr && c = `Sum sumexpr) -> true
  | _ -> false

let%test "deftype_sum_all_compact" =
  let parsed =
    parse_string ~consume:All exp_p
    "(deftype (a b c) (where (a is [Eq]) (b is [Ord, Show])) (Some of x, None ))" in
  let texpr = `Single (["a"; "b"], "c") in
  let whereexpr = ["a", ["Eq"]; "b", ["Ord"; "Show"]] in
  let sumexpr = [("Some", Some (`Single ([], "x"))); ("None", None)] in
  let open Stdlib in
  match parsed with
  | Ok (`DefType (a, b, c)) when (a = texpr && b = whereexpr && c = `Sum sumexpr) -> true
  | _ -> false

let%test "deftype_prod_all_compact" =
  let parsed =
    parse_string ~consume:Prefix exp_p
    "(deftype (a b c) (where (a is [Eq]) (b is [Ord, Show])) {Some of x, None of a b s})" in
  let texpr = `Single (["a"; "b"], "c") in
  let whereexpr = ["a", ["Eq"]; "b", ["Ord"; "Show"]] in
  let prodexpr = [("Some", `Single ([], "x")); ("None", `Single (["a"; "b"], "s"))] in
  let open Stdlib in
  match parsed with
  | Ok (`DefType (a, b, c)) when (a = texpr && b = whereexpr && c = `Prod prodexpr) -> true
  | _ -> false
