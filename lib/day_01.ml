let to_int str =
  let sign =
    match String.get str 0 with
    | 'R' -> 1
    | 'L' -> -1
    | _ -> failwith "Invalid sign"
  in
  Utils.str_tail 1 str |> int_of_string |> ( * ) sign

let to_position it = match it mod 100 with n when n < 0 -> n + 100 | n -> n

let solve_1 acc it =
  let counter, pos = acc in
  let new_pos = to_position (pos + it) in
  match new_pos with 0 -> (counter + 1, 0) | _ -> (counter, new_pos)

let solve_2 acc it =
  let counter, pos = acc in
  let new_pos = pos + it in
  let add_zero_cross n = n + Utils.int_of_bool (pos > 0 && new_pos <= 0) in
  let zero_crossings = new_pos / 100 |> abs |> add_zero_cross in
  let new_pos = to_position new_pos in
  (counter + zero_crossings, new_pos)

let solution solve lines =
  lines |> List.map to_int |> List.fold_left solve (0, 50) |> fun acc ->
  fst acc |> Utils.print 1

let run file =
  let lines =
    Utils.readlines ("inputs/" ^ file ^ ".txt") |> Utils.drop_empty_lines
  in
  solution solve_1 lines;
  solution solve_2 lines
