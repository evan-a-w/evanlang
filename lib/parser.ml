open Types

let getchar = Scanf.scanf "%c" (fun x -> x)

type lex_state = 
  | Awaiting
  | Number 
  | Symbol 
  | String 
  | List 
  | Array 
  | Identifier

type lex_result =
  | Number of string
  | Symbol of string
  | String of string
  | List of lex_result list
  | Array of lex_result list
  | Identifier of string

type lexer = {c: char; prog: program; }
