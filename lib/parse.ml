(* open Types *)

let try_fail f x = match x with
  | None -> f x
  | Some y -> Some y

type 'a parse_func = string -> 'a option * string

let fail p = fun s ->
  match p s with
    | (None, _) -> (None, s)
    | o -> o

let (<<) : 'a parse_func -> 'b parse_func -> 'b parse_func =
  fun a b -> fun s ->
    let (_, s') = s |> fail a in
    b s'

let (>>) : 'a parse_func -> 'b parse_func -> 'a parse_func =
  fun a b -> fun s ->
    let (x, s') = a s in
    let (_, s'') = s' |> fail b in
    (x, s'')

let (<|>) : 'a parse_func -> 'a parse_func -> 'a parse_func =
  fun a b -> fun s ->
    let (x, s') = a s in
    match x with
      | None -> let (y, s'') = b s in (y, s'')
      | Some y -> (Some y, s')

let dropwhile_string f s =
  let rec aux i len =
    if i < len && f s.[i] then
      aux (i + 1) len
    else
      String.sub s i len in
  aux 0 (String.length s)

let ws : unit parse_func = fun s ->
  (Some (), dropwhile_string (fun x -> x = ' ' || x = '\t' || x = '\n') s)

let chara c = fun s ->
  if String.length s > 0 && s.[0] = c then
    (Some (), String.sub s 1 (String.length s))
  else
    (None, s)

let match_string str = fun s ->
  if try s = String.sub str 0 (String.length s) with
    Invalid_argument _ -> false then
    (Some (), String.sub s (String.length str) (String.length s))
  else (None, s)

let rev l =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs in
  aux [] l

let many0 : 'a parse_func -> ('a list) parse_func = fun p -> fun s ->
  let rec aux acc cs = match p cs with
    | (None, _) -> (Some (rev acc), cs)
    | (Some v, s') -> aux (v :: acc) s' in
  aux [] s

let many1 : 'a parse_func -> ('a list) parse_func = fun p -> fun s ->
  let rec aux acc cs = match p cs with
    | (None, _) ->
      (match rev acc with
        | [] -> (None, cs)
        | _ -> (Some acc, cs))
    | (Some v, s') -> aux (v :: acc) s' in
  aux [] s

let matches p = fun s -> 
  let (first, _) = p s in
  (first, s)

let (<$>) f p = fun s ->
  match p s with
  | (None, s') -> (None, s')
  | (Some v, s') -> (Some (f v), s')

let split_string f s =
  let rec aux i len =
    if i < len && f s.[i] then
      aux (i + 1) len
    else
      (String.sub s 0 i, String.sub s i len) in
  aux 0 (String.length s)

let split_with f = fun s ->
  let (first, second) = split_string f s in
  (Some first, second)

let nonempty : string parse_func -> string parse_func = fun p s ->
  let (fir, sec) = p s in
  match fir with
    | Some "" -> (None, sec)
    | _ -> (fir, sec)

let string_has s c =
  let rec aux i =
    if i < String.length s then
      s.[i] = c || aux (i + 1)
    else false
  in aux 0

let split_parser f s =
  let (first, second) = split_string f s in
  (Some first, second)

let one_of s = split_parser (string_has s)
