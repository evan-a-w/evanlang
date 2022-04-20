open Lexer
open Parse

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

let parsed s =
  let reader = new string_reader s in
  parse reader

let assert_parse_eq s rhs = 
  if parsed s != rhs then

    failwith "parse failed"
  else ()

let test_basic_lex () = assert_lex_eq "abc" (IdentifierLit "abc")

let test_sexpr_lex () = assert_lex_eq "(abc 123)" (SexprLit [IdentifierLit "abc"; NumberLit "123"])

let test_list_lex () = assert_lex_eq "'(abc, 123)" (ListLit [IdentifierLit "abc"; NumberLit "123"])

let test_list_fail_lex () = assert_exception "'(abc 123)"

let test_list_parse () =
  assert_parse_eq 
    "'(abc, 123, 0.0)" 
    (Ok [List [Identifier "abc"; Int 123; Float 0.0]])

let run_tests () =
  test_basic_lex ();
  test_list_lex ();
  test_sexpr_lex ();
  test_list_fail_lex ();
  test_list_parse ();
  Printf.printf "Tests passed!\n"
