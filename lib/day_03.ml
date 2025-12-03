let find_biggest_two_digets line =
  let len = String.length line in
  let rec aux acc pos =
    if pos >= len then acc
    else
      let next = pos + 1 in
      match (acc, String.get line pos) with
      | [], ch -> aux [ ch ] next
      (* | a :: _, ch when ch > a && next < len -> aux [ch] next *)
      | [ a ], ch -> aux [ a; ch ] next
      | [ a; b ], ch when b > a -> aux [ b; ch ] next
      | [ a; b ], ch when ch > b -> aux [ a; ch ] next
      | acc, _ -> aux acc next
  in
  aux [] 0 |> Utils.string_of_chars |> int_of_string

let run file =
  let lines =
    Utils.readlines ("inputs/" ^ file ^ ".txt")
    |> Utils.drop_empty_lines |> List.map String.trim
  in
  lines |> List.map find_biggest_two_digets |> Utils.sum_int |> Utils.print 3;

