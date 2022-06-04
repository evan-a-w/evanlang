type exp =
  [ `Unit
  | `Bool of bool
  | `FloatLit of float
  | `IntLit of int
  | `StringLit of string
  | `Identifier of string
  | `SymbolLit of string
  | `ListLit of exp list
  | `ArrayLit of exp array
  | `Call of exp * (exp list)
  | `DefType of type_expr * trait_spec * type_lit
  | `Typed of type_expr * exp ]
  (* TODO: def trait needs to be done *)
  (* TODO: let expr and function *)
and type_lit =
  [ `Sum of (string * (type_expr option)) list
  | `Prod of (string * type_expr) list ]
(* TODO: add tuple support *)
and type_expr =
  [ `Single of (string list) * string
  | `Multi of type_expr list ]
and trait_spec = (string * (string list)) list

let string_of_type = function
  | `Unit -> "Unit"
  | `Bool _ -> "Bool"
  | `FloatLit _ -> "Float"
  | `IntLit _ -> "Int"
  | `StringLit _ -> "String"
  | `Identifier _ -> "Identifier"
  | `SymbolLit _ -> "Symbol"
  | `ListLit _ -> "List"
  | `ArrayLit _ -> "Array"
  | `VarDecl _ -> "VarDecl"
  | `Call _ -> "Call"
  | `Typed _ -> "Typed"
  | `DefType _ -> "DefType"

let print_type t = Printf.printf "%s\n" (string_of_type t)
