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
  | `TypeLit of type_lit
  | `Call of exp * (exp list) ]
and type_lit =
  | Sum of (string, exp) Hashtbl.t
  | Prod of (string, string) Hashtbl.t
