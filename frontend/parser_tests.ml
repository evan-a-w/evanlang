open Angstrom
open Parser
open Core

let%test "int_succ" =
  let parsed = parse_string ~consume:All int_p "1" in
  match parsed with
  | Error e -> (
      fprintf stderr "%s" e;
      false
    )
  | Ok i ->
    if i = 1 then true else (
      fprintf stderr "%d" i;
      false
    )

let%test "float_succ" =
  let parsed = parse_string ~consume:All float_p "1.5" in
  match parsed with
  | Error e -> (
      fprintf stderr "%s" e;
      false
    )
  | Ok f ->
    if Float.equal f 1.5  then true else (
      fprintf stderr "%f" f;
      false
    )
