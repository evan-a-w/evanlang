open Type_sys

type middle_exp =
  ([ `Unit
  | `Bool of bool
  | `FloatLit of float
  | `IntLit of int
  | `StringLit of string
  | `SymbolLit of string
  | `Identifier of string
  | `ListLit of middle_exp list
  | `ArrayLit of middle_exp array
  | `Function of ((string list) * middle_exp)
  | `Call of (middle_exp * (middle_exp list)) ]) * Types.typ
