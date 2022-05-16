open Angstrom

let is_digit c = let code = Char.code c in
  Char.code '0' <= code && code <= Char.code '9'

let digit = satisfy is_digit

let int_p = lift int_of_string (take_while1 is_digit)

let float_p = 
  lift3 (fun x y z -> float_of_string (x ^ y ^ z))
        (take_while1 is_digit)
        (string ".")
        (take_while is_digit)

let exp_p = fail "Unimplemented"
