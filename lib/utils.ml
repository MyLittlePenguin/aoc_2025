(* io *)
let readfile filename =
  let ic = In_channel.open_text ("inputs/" ^ filename ^ ".txt") in
  In_channel.input_all ic

let readlines filename = readfile filename |> String.split_on_char '\n'

(* Parsing stuff *)
let split_by_empty_lines lines =
  let rec aux acc current_sub_list lines =
    match lines with
    | [] -> List.map List.rev (current_sub_list :: acc) |> List.rev
    | "" :: tl -> aux (current_sub_list :: acc) [] tl
    | hd :: tl -> aux acc (hd :: current_sub_list) tl
  in
  aux [] [] lines

let drop_empty_lines = List.filter (function "" -> false | _ -> true)
let str_tail pos str = String.sub str pos (String.length str - pos)

let repeat n str =
  let rec aux acc = function 0 -> acc | n -> aux (acc ^ str) (n - 1) in
  aux "" n

(* List stuff *)
let range first last = List.init (last - first + 1) (fun it -> first + it)
let sum_int = List.fold_left ( + ) 0
let sum_float = List.fold_left ( +. ) 0.0
let list_contains needle list =
  let rec aux = function
    | [] -> false
    | hd :: _ when hd = needle -> true
    | _ :: tl -> aux tl
  in aux list
    

(* Other stuff *)
let int_of_bool = function true -> 1 | false -> 0
let string_of_chars chars = String.init (List.length chars) (List.nth chars)
let string_of_char char = String.make 1 char
let print day result = Printf.printf "Day %02d: %d\n" day result
