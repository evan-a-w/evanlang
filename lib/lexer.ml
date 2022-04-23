open Types

let getchar () = Scanf.scanf "%c" (fun x -> x)

class virtual stream_reader = object
  method virtual get_char: char
  method virtual unget_char: char -> unit
end

class stdin_reader = object
  inherit stream_reader
  val mutable c: char option = None

  method get_char = match c with
    | None -> getchar ()
    | Some x ->
      c <- None;
      x

  method unget_char x = c <- Some x
end

exception EOF

exception ConstructionError of char * string

let catch_cons f = try Ok (f ()) with
    ConstructionError (expected, s) -> Error (ParseError {
                                         expected = Printf.sprintf "'%c'" expected; 
                                         explanation = s
                                       })

class string_reader str = object
  inherit stream_reader
  val mutable index = 0 
  val str = str

  method get_char =
    if index < String.length str then
      let res = str.[index] in
      index <- index + 1;
      res
    else raise End_of_file

  method unget_char _ = if index > 0 then
                          index <- index - 1
                        else
                          ()
end

type lex_result =
  | NumberLit of string
  | SymbolLit of string
  | StringLit of string
  | SexprLit of lex_result list
  | ListLit of lex_result list
  | IdentifierLit of string

let join_list str l =
  let rec aux acc cl = match cl with
    | x::xs -> aux (acc ^ str ^ x) xs
    | [] -> acc in
  match l with
    | x :: xs -> x ^ aux "" xs
    | [] -> ""

let rec string_of_lex_result x = Printf.(match x with
  | NumberLit s -> sprintf "%s." s
  | SymbolLit s -> sprintf "'%s" s
  | StringLit s -> sprintf "\"%s\"" s
  | SexprLit l -> sprintf "(%s)" (join_list ", " (List.map string_of_lex_result l))
  | ListLit l -> sprintf "'(%s)" (join_list ", " (List.map string_of_lex_result l))
  | IdentifierLit s -> sprintf "%s" s)

let void _ = ()

class string_state type_constructor char_allowed finaliser = object
  val mutable curr_str: char list = []
  val type_cons: string -> lex_result = type_constructor
  val char_allow: char -> bool = char_allowed
  val mutable f: stream_reader -> unit = finaliser
  
  method finalise reader =
    f reader;
    type_constructor (String.of_seq (List.to_seq (List.rev curr_str)))

  method add_char x = curr_str <- x :: curr_str

  method char_allowed = char_allowed
end

class list_state type_constructor end_char sep_func = object
  val mutable curr_list: lex_result list = []
  val type_cons: lex_result list -> lex_result = type_constructor
  val end_char: char = end_char
  
  method finalise: stream_reader -> lex_result = fun _ -> type_constructor (List.rev curr_list)

  method add_val x = curr_list <- x :: curr_list

  method end_char = end_char

  method get_sep: stream_reader -> unit = sep_func
end

type lex_state = 
  | Awaiting
  | StringState of string_state
  | ListState of list_state
  | Done

type single_lexer = {
  token: lex_result option;
  state: lex_state;
  reader: stream_reader;
}

let whitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

let delimiter c =
  c = '(' || c = ')' || c = '[' || c = ']' || c = '{' || c = '}' || c = ','
  || c = ';' || c = ':' || c = '"'

let identifier c =
  c = '_' || c = '-' || c = '+' || c = '*' || c = '/' || c = '<' || c = '>'
  || c = '=' || c = '!' || c = '?' || c = ':' || c = '$' || c = '%' || c = '&'
  || c = '~' || c = '^'
  || Char.code 'a' <= Char.code c && Char.code c <= Char.code 'z'
  || Char.code 'A' <= Char.code c && Char.code c <= Char.code 'Z'

let is_number c = Char.code '0' <= Char.code c && Char.code c <= Char.code '9'

let get_string_state_funcs: char -> stream_reader -> (((string -> lex_result)
                                                       * (char -> bool)
                                                       * (stream_reader -> unit)) option)
  = fun c r ->
  if identifier c then
    Some ((fun x -> IdentifierLit x), identifier, void)
  else if is_number c then
    Some ((fun x -> NumberLit x), is_number, void)
  else if c = '"' then
    Some ((fun x -> StringLit x)
          , (fun c -> c <> '"')
          , (fun x -> try
                        let c = x#get_char in
                        if c = '"' then
                          ()
                        else raise End_of_file
                      with End_of_file -> raise (ConstructionError (c, "Unterminated string"))))
  else if c = '\'' then
    let c = r#get_char in
    if delimiter c then
      (r#unget_char c;
       None)
    else (
      r#unget_char c;
      Some ((fun x -> SymbolLit x), identifier, void))
  else
    None

let rec comma_sep reader =
  let c = reader#get_char in
  if c = ',' then
    ()
  else if whitespace c then
    comma_sep reader
  else
    raise (ConstructionError (c, "Expected comma or whitespace"))

let get_list_state_funcs: char -> stream_reader -> (((lex_result list -> lex_result)
                                                     * char
                                                     * (stream_reader -> unit))
                                                    option)
  = fun c r -> if c = '(' then
    Some ((fun x -> SexprLit x), ')', (fun _ -> ()))
  else if c = '\'' then
    let c = r#get_char in
    if c <> '(' then
      (r#unget_char c;
       None)
    else Some ((fun x -> ListLit x), ')', comma_sep)
  else
    None

let iter_string_state: single_lexer -> string_state -> single_lexer = fun lex st ->
    try
      let c = lex.reader#get_char in
      if whitespace c then
        {
          token = Some (st#finalise lex.reader);
          state = Done;
          reader = lex.reader;
        }
      else if st#char_allowed c then
        (st#add_char c;
        {
          token = None;
          state = StringState st;
          reader = lex.reader;
        })
      else
       (lex.reader#unget_char c;
        raise End_of_file)
    with End_of_file ->
      {
        token = Some (st#finalise lex.reader);
        state = Done;
        reader = lex.reader;
      }

let rec iter_list_state: single_lexer -> list_state -> single_lexer = fun lex st ->
  try
    let tok = get_one_token lex.reader in
    st#add_val tok;
    st#get_sep lex.reader;
    {
      token = None;
      state = ListState st;
      reader = lex.reader;
    }
  with
    ConstructionError (c, _) when c = st#end_char -> {
      token = Some (st#finalise lex.reader);
      state = Done;
      reader = lex.reader;
    }
and transition lex = match lex.state with
  | Done -> lex
  | Awaiting ->
    let c = lex.reader#get_char in
    if whitespace c then
      lex
    else (match get_string_state_funcs c lex.reader with
      | Some (res_cons, char_allowed, finaliser) -> {
          token = None;
          state = StringState (
            let st = new string_state res_cons char_allowed finaliser in
            st#add_char c;
            st
          );
          reader = lex.reader;
        }
      | None -> match get_list_state_funcs c lex.reader with 
        | Some (res_cons, end_char, sep_func) -> {
            token = None;
            state = ListState (new list_state res_cons end_char sep_func);
            reader = lex.reader;
          }
        | None -> raise (ConstructionError (c, Printf.sprintf "Unexpected character '%c'" c)))
  | StringState st -> iter_string_state lex st
  | ListState st -> iter_list_state lex st
and get_one_token reader =
  let lex = {
    token = None;
    state = Awaiting;
    reader = reader;
  } in
  let rec aux l = match l.state with
    | Done -> unwrap l.token
    | _ -> aux (transition l) in
  aux lex

type ast = lex_result list

let get_rest_of_reader reader =
  let rec aux acc = 
    try aux (reader#get_char :: acc) with End_of_file -> acc
  in String.of_seq (List.to_seq (List.rev (aux [])))

let read_ast reader =
  let next () = catch_cons (fun () -> get_one_token reader) in
  let rec aux acc = match next () with
    | Ok v -> aux (v :: acc)
    | Error _ -> match get_rest_of_reader reader with
      | "" -> Ok (List.rev acc)
      | s ->  Error (
          ParseError {
            expected = "EOF"; 
            explanation = Printf.sprintf "Received string: \"%s\"" s
          }
        )
  in Result.map List.rev (aux [])

