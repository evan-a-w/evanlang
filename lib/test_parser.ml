open Lexer

let assert_lex_eq s rhs =
  let reader = new string_reader s in
  assert (get_one_token reader = rhs)

let assert_exception_eq s rhs =
  let reader = new string_reader s in
  try void (get_one_token reader); assert false with
    | e -> assert (e = rhs)

let assert_exception s =
  let reader = new string_reader s in
  try void (get_one_token reader); assert false with
    | _ -> ()

let test_basic_lex () = assert_lex_eq "abc" (Identifier "abc")

let test_sexpr_lex () = assert_lex_eq "(abc 123)" (Sexpr [Identifier "abc"; Number "123"])

let test_list_lex () = assert_lex_eq "'(abc, 123)" (List [Identifier "abc"; Number "123"])

let test_list_fail_lex () = assert_exception "'(abc 123)"

let run_tests () =
  test_basic_lex ();
  test_list_lex ();
  test_sexpr_lex ();
  test_list_fail_lex ();
  Printf.printf "Tests passed!\n"
