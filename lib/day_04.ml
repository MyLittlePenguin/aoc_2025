let is_role = function '@' -> true | _ -> false

let check_neighbours row col lines =
  let line_length = String.length lines.(0) in
  let line_count = Array.length lines in
  [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
  |> List.map (fun (dx, dy) -> (col + dx, row + dy))
  |> List.filter (fun (x, y) ->
         x >= 0 && y >= 0 && x < line_length && y < line_count)
  |> List.map (fun (x, y) -> String.get lines.(y) x)
  |> List.filter is_role |> List.length

let check_all_fields lines =
  let new_lines =
    Array.mapi
      (fun y line ->
        String.mapi
          (fun x ch ->
            if is_role ch then
              match check_neighbours y x lines with
              | n when n < 4 -> 'x'
              | _ -> ch
            else ch)
          line)
      lines
  in
  new_lines

let to_str arr = arr |> Array.to_list |> String.concat ""

let count_removed_roles lines =
  lines |> to_str
  |> String.fold_left (fun count it -> if it = 'x' then count + 1 else count) 0

let run file =
  let lines =
    Utils.readlines file |> List.map String.trim |> Utils.drop_empty_lines
    |> Array.of_list
  in
  check_all_fields lines |> count_removed_roles |> Utils.print 4;
  let rec aux_02 lines =
    let new_lines = check_all_fields lines in
    if new_lines <> lines then aux_02 new_lines else lines
  in
  aux_02 lines |> count_removed_roles |> Utils.print 4
