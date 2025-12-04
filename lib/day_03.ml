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

let find_biggest_12_digets line =
  let len = String.length line in
  let last = 11 in
  let add_digit acc ch =
    let rec aux acc pos =
      let previous = pos - 1 in
      match (String.get acc pos, String.get acc previous) with
      | c, p when c > p ->
          let hd = String.sub acc 0 previous in
          let tail = Utils.str_tail pos acc in
          let new_ch =  Utils.string_of_char ch in
          hd ^ tail ^ new_ch
      | c, _ when pos >= last && ch > c ->
          String.sub acc 0 last ^ Utils.string_of_char ch
      | _, _ when pos >= last -> acc
      | _, _ -> aux acc (pos + 1)
    in
    if String.length acc < 12 then acc ^ Utils.string_of_char ch else aux acc 1
  in
  let rec aux pos acc =
    if pos >= len then acc
    else
      let next = pos + 1 in
      let ch = String.get line pos in
      add_digit acc ch |> aux next
  in
  aux 0 ""

let run file =
  let lines =
    Utils.readlines ("inputs/" ^ file ^ ".txt")
    |> Utils.drop_empty_lines |> List.map String.trim
  in
  lines |> List.map find_biggest_two_digets |> Utils.sum_int |> Utils.print 3;
  lines
  |> List.map find_biggest_12_digets
  |> List.map int_of_string |> Utils.sum_int |> Utils.print 3
