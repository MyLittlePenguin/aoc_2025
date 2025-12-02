let range first last = List.init (last - first + 1) (fun it -> first + it)

let to_range it =
  let dash_pos = String.index it '-' in
  let first = String.sub it 0 dash_pos in
  let first = first |> int_of_string in
  let last = String.sub it (dash_pos + 1) (String.length it - dash_pos - 1) in
  let last = last |> int_of_string in
  range first last

let is_invalid it =
  let numstr = string_of_int it in
  let len = String.length numstr in
  let half_len = String.length numstr / 2 in
  let first = String.sub numstr 0 half_len in
  let last = String.sub numstr half_len half_len in
  half_len * 2 = len && first = last

let filter_invalid r = List.filter is_invalid r

let run file =
  let ranges =
    Utils.readfile ("inputs/" ^ file ^ ".txt")
    |> String.trim |> String.split_on_char ',' |> List.map to_range
  in
  List.map filter_invalid ranges
  |> List.flatten |> Utils.sum_int |> Utils.print 2;
