let to_range it =
  let dash_pos = String.index it '-' in
  let first = String.sub it 0 dash_pos |> int_of_string in
  let last =
    String.sub it (dash_pos + 1) (String.length it - dash_pos - 1)
    |> int_of_string
  in
  Utils.range first last

let is_invalid_1 it =
  let numstr = string_of_int it in
  let len = String.length numstr in
  let half_len = String.length numstr / 2 in
  let first = String.sub numstr 0 half_len in
  let last = String.sub numstr half_len half_len in
  half_len * 2 = len && first = last

let is_invalid_2 it =
  let numstr = string_of_int it in
  let len = String.length numstr in
  let half_len = String.length numstr / 2 in
  let rec aux pattern_len =
    let part_count = if pattern_len = 0 then 0 else len / pattern_len in
    let pattern = String.sub numstr 0 pattern_len in
    match pattern_len with
    | 0 -> false
    | _ when numstr = Utils.repeat part_count pattern -> true
    | pl -> aux (pl - 1)
  in
  aux half_len

let run file =
  let ranges =
    Utils.readfile ("inputs/" ^ file ^ ".txt")
    |> String.trim |> String.split_on_char ',' |> List.map to_range
  in
  List.map (List.filter is_invalid_1) ranges
  |> List.flatten |> Utils.sum_int |> Utils.print 2;
  List.map (List.filter is_invalid_2) ranges
  |> List.flatten |> Utils.sum_int |> Utils.print 2
