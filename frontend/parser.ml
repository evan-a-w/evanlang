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

let inside_string = let open Angstrom.Let_syntax in
  (* for some reason one cant be recursive so use a ref *)
  let f = ref (any_char >>= fun _ -> return ' ') in
  let one = match%bind any_char with
  | '"' -> fail "should not terminate inside one"
  | '\\' ->
    (match%bind any_char with
    | '"' -> '"' |> return
    | '\\' -> '\\' |> return
    | 'n' -> '\n' |> return
    | 't' -> '\t' |> return
    | 'r' -> '\r' |> return
    | 'b' -> '\b' |> return
    | '\n' | '\t' | ' ' -> !f
    | c -> return c)
  | c -> return c in
  let terminated = match%bind any_char with
    | '"' -> return '"'
    | _ -> fail "unterminated" in
  f := one;
  Base.String.of_char_list <$> many_till one terminated

let string_p = char '"' *> inside_string

let bool_p = 
  string "true" *> return true <|> string "false" *> return false

let unit_p = string "()" *> return ()

let is_identifier c = let code = Char.code and cc = Char.code c in
  code 'a' <= cc && cc <= code 'z'
  || code 'A' <= cc && cc <= code 'Z'
  || code '0' <= cc && cc <= code '9'
  || c = '_' || c = '\'' || c = '.' || c = '!' || c = '?' || c = '$' || c = '%' 
  || c = '&' || c = '*' || c = '+' || c = '-' || c = '/' || c = ':' || c = '<'
  || c = '=' || c = '>' || c = '^' || c = '|' || c = '~'

let identifier_p = take_while is_identifier

let symbol_p = char '\'' *> identifier_p

let whitespace = satisfy (fun x -> x = ' ' || x = '\t' || x = '\n' || x = '\r')

let skip_whitespace = skip_many whitespace
let skip_whitespace1 = skip_many1 whitespace

let call_p exp_p = let open Angstrom.Let_syntax in
  let%bind e = char '(' *> skip_whitespace *> exp_p in
  let%bind es = sep_by skip_whitespace1 exp_p in
  char ')' *> return (`Call (e, es))

let exp_p =
  unit_p *> return `Unit
  <|> (bool_p >>= fun b -> return (`Bool b))

let exp_p = fail "Unimplemented"
