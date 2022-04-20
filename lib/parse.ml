open Lexer
open Types

type parse_error = 
  | TypeError 

let parse =
  let parse_ast = match 
