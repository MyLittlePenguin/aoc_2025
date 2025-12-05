exception ParseFreshException

let is_in_range it (a, b) = it >= a && it <= b

let run file =
  let lines = Utils.readlines file |> Utils.split_by_empty_lines in
  let fresh_lines = List.nth lines 0 in
  let available_lines = List.nth lines 1 in
  let fresh =
    List.map (String.split_on_char '-') fresh_lines
    |> List.map (function
         | [ a; b ] -> (int_of_string a, int_of_string b)
         | _ -> raise ParseFreshException)
  in
  let available = List.map int_of_string available_lines in
  List.filter (fun it -> List.exists (is_in_range it) fresh) available
  |> List.length |> Utils.print 5;

  let fresh = List.sort (fun a b -> fst a - fst b) fresh in
  let rec aux acc last_range ranges =
    match (last_range, ranges) with
    | last, [] -> last :: acc
    | (_, b1), (a2, b2) :: tl when a2 <= b1 && b2 <= b1 -> aux acc last_range tl
    | (a1, b1), (a2, b2) :: tl when a2 <= b1 -> aux acc (a1, b2) tl
    | last, hd :: tl -> aux (last :: acc) hd tl
  in
  match fresh with
  | [] -> print_endline "Not Possible"
  | fresh_hd :: fresh_tl ->
      aux [] fresh_hd fresh_tl
      |> List.map (fun (a, b) -> b - a + 1)
      |> Utils.sum_int |> Utils.print 5
