open Lexer
open Types
open Stdlib

let result_chain a f1 f2 =
  match f1 a with
    | Ok x -> Ok x
    | Error _ -> f2 a

let parse_lexed: lex_result list -> (type_instance list, parse_error) result =
  let rec parse_tok: lex_result -> (type_instance, parse_error) result = function
    | SymbolLit s -> Ok (Symbol s)
    | IdentifierLit s -> Ok (Identifier s)
    | StringLit s -> Ok (String s)
    | NumberLit s ->
      let read_int a = Option.map (fun x -> Int x) (int_of_string_opt a) in
      let read_float a = Option.map (fun x -> Float x) (float_of_string_opt a) in
      let res_read_int x =
        Option.to_result ~none:(LitError {expected = Int_; found = s})
                         (read_int x) in
      let res_read_float x =
        Option.to_result ~none:(LitError {expected = Float_; found = s})
                         (read_float x) in
      result_chain s res_read_int res_read_float
    | ListLit l -> Result.map (fun x -> List x) (parse_list l)
    | SexprLit l -> Result.map (fun x -> Sexpr x) (parse_list l)
  and parse_list = function
    | [] -> Ok []
    | (x :: xs) -> let parsed = parse_tok x in
      match parsed with
        | Ok y -> Result.map (fun ys -> y :: ys) (parse_list xs)
        | Error e -> Error e in
  function
  | [] -> Error (Malformed "Empty Sexpr")
  | o -> parse_list o

let parse: stream_reader -> ((type_instance list, parse_error) result) =
  fun r -> Result.bind (read_ast r) parse_lexed

let gen_types r = parse r |> List.map (fun x -> (x, type_of x))
