let readlines filename =
  let ic = In_channel.open_text filename in
  In_channel.input_all ic |> String.split_on_char '\n'

let drop_empty_lines = List.filter (function "" -> false | _ -> true)
let str_tail pos str = String.sub str pos (String.length str - pos)
let print day result = Printf.printf "Day %02d: %d\n" day result
let int_of_bool = function true -> 1 | false -> 0
let sum_int = List.fold_left ( + ) 0
let sum_float = List.fold_left ( +. ) 0.0
