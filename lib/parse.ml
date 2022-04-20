open Lexer
open Types
open Stdlib

let result_chain a f1 f2 =
  match f1 a with
    | Ok x -> Ok x
    | Error _ -> f2 a

let parse: lex_result list -> (type_instance list, parse_error) result =
  let rec parse_tok: lex_result -> (type_instance, parse_error) result = function
    | Symbol s -> Ok (Symbol s)
    | Identifier s -> Ok (Identifier s)
    | String s -> Ok (String s)
    | Number s ->
      let read_int a = Option.map (fun x -> Int x) (int_of_string_opt a) in
      let read_float a = Option.map (fun x -> Float x) (float_of_string_opt a) in
      let res_read_int x =
        Option.to_result ~none:(LitError {expected = Int; found = s})
                         (read_int x) in
      let res_read_float x =
        Option.to_result ~none:(LitError {expected = Float; found = s})
                         (read_float x) in
      result_chain s res_read_int res_read_float
    | List l -> Result.map (fun x -> List x) (parse_list l)
    | Sexpr l -> Result.map (fun x -> List x) (parse_list l)
  and parse_list = function
    | [] -> Ok []
    | (x :: xs) -> let parsed = parse_tok x in
      match parsed with
        | Ok y -> Result.map (fun ys -> y :: ys) (parse_list xs)
        | Error e -> Error e in
  parse_list
