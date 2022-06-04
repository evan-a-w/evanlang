open Angstrom
open Types

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

let identifier_p = take_while1 is_identifier >>= function
  | "->" -> fail "-> is reserved"
  | o -> return o

let symbol_p = char '\'' *> identifier_p

let whitespace = satisfy (fun x -> x = ' ' || x = '\t' || x = '\n' || x = '\r')

let skip_whitespace = skip_many whitespace
let skip_whitespace1 = skip_many1 whitespace

let ws = skip_whitespace
let ws1 = skip_whitespace1

let comma_sep = ws *> char ',' *> ws

let call_p exp_p =
  (fun x -> `Call x)
  <$> ws *> char '(' *> ws *> both (exp_p <* ws) (sep_by ws1 exp_p) <* ws <* char ')'

let trait_list_p = let open Angstrom.Let_syntax in
  ws *> char '[' *>
  let%bind res = sep_by comma_sep identifier_p in
  ws *> char ']' *> return res

let trait_spec_p = let open Angstrom.Let_syntax in
  let one = 
    ws *> char '(' *> ws *>
    let%bind type_param = identifier_p in
    ws *> string "is" *> ws1 *>
    let%bind tl = trait_list_p in
    ws *> char ')' *> return (type_param, tl) in
  ws *> char '(' *> ws *> string "where" *> ws1 *>
  sep_by ws1 one >>= fun res -> ws *> char ')' *> return res

(* TODO: broken - need to take one if can (no arrow following) otherwise not *)
let type_expr_p : type_expr Angstrom.t = fix (fun f ->
  let one =
    let rec aux = function
      | [] -> (* unreachable *) raise Lazy.Undefined
      | [x] -> ([], x)
      | x :: xs ->
        let (first, last) = aux xs in
        (x :: first, last) in
    (fun x -> `Single (aux x)) <$> many (ws *> identifier_p <* ws) in
  ((fun x -> `Multi x) <$> sep_by1 (ws1 *> string "->" *> ws1)
            (one <|> (char '(' *> ws *> f <* ws <* char ')')))
  <|> one)

let sum_type_p = let open Angstrom.Let_syntax in
  let one =
    let%bind id = identifier_p in
    let next = ws1 *> string "of" *> ws1 *> type_expr_p >>= fun x -> return (id, Some x) in
    option (id, None) next in
  ws *> char '(' *> sep_by comma_sep one <* ws <* char ')'

let prod_type_p =
  let one = both identifier_p (ws *> string "of" *> ws *> type_expr_p) in
  ws *> char '{' *> sep_by comma_sep one <* ws <* char '}'

let deftype_expr = let open Angstrom.Let_syntax in
  let name_thing =
    (fun x -> `Single ([], x)) <$> identifier_p
    <|> (char '(' *> ws *> type_expr_p <* ws <* char ')') in
  let sum_thing = (fun x -> `Sum x) <$> sum_type_p in
  let prod_thing = (fun x -> `Prod x) <$> prod_type_p in
  let inner =
    let%bind texpr = name_thing in
    let%bind where_clause = option [] trait_spec_p in
    let%bind last = sum_thing <|> prod_thing in
    return (`DefType (texpr, where_clause, last)) in
  char '(' *> ws *> string "deftype" *> ws *>
  let%bind res = inner in
  ws *> char ')' *> return res
  
let list_p exp_p = let open Angstrom.Let_syntax in
  string "'(" *>
  let%bind l = sep_by ws1 exp_p in
  ws *> char ')' *> return (`ListLit l)

let array_p exp_p = let open Angstrom.Let_syntax in
  char '[' *>
  let%bind l = sep_by ws1 exp_p in
  ws *> char ']' *> return (`ArrayLit (Array.of_list l))

let exp_p : exp Angstrom.t = fix (fun f ->
  unit_p *> return `Unit
  <|> ((fun b -> `Bool b) <$> bool_p)
  <|> ((fun f -> `FloatLit f) <$> float_p)
  <|> ((fun i -> `IntLit i) <$> int_p)
  <|> ((fun s -> `StringLit s) <$> string_p)
  <|> ((fun s -> `SymbolLit s) <$> symbol_p)
  <|> deftype_expr
  <|> list_p f
  <|> array_p f
  <|> call_p f
  <|> ((fun s -> `Identifier s) <$> identifier_p))
