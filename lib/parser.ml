open Parse

let delimiters = "();\"'`|[]{}"
let whitespace = " \t\n\r"

let identifier = nonempty @@ split_parser (fun c ->
    string_has "+-.*/<=>!?:$%_&~^" c
    || Char.code 'a' <= Char.code c  && Char.code c <= Char.code 'z'
    || Char.code 'A' <= Char.code c  && Char.code c <= Char.code 'Z')

let str = chara '"' << identifier >> chara '*'

let symbol = chara '\'' << identifier
