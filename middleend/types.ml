open Type_sys

type middle_exp =
  [ `Unit
  | `Bool of bool
  | `FloatLit of float
  | `IntLit of int
  | `StringLit of string
  | `SymbolLit of string
  | `Identifier of string * Types.typ
  | `ListLit of middle_exp list * Types.typ
  | `ArrayLit of middle_exp array * Types.typ
  | `Function of ((string list) * middle_exp) * Types.typ
  | `Call of (middle_exp * (middle_exp list)) * Types.typ ]
