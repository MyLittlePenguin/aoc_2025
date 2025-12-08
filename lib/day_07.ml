let is_beam line pos =
  pos >= 0 && String.length line > pos && String.get line pos = '|'

let is_splitter line pos =
  pos >= 0 && String.length line > pos && String.get line pos = '^'

let run_beams lines =
  let line_count = Array.length lines in
  let aux last_line line =
    String.mapi
      (fun i ch ->
        let next = i + 1 in
        let prev = i - 1 in
        match (ch, String.get last_line i) with
        | _, '.' when is_splitter line next && is_beam last_line next -> '|'
        | _, '.' when is_splitter line prev && is_beam last_line prev -> '|'
        | '.', '|' -> '|'
        | '.', 'S' -> '|'
        | '^', '|' -> '^'
        | ch, _ -> ch)
      line
  in
  Array.sub lines 1 (line_count - 1)
  (* i is offset because first line is skipped *)
  |> Array.fold_left
       (fun (i, passed_lines) line ->
         match passed_lines with
         | [] -> raise Utils.ElfPanic
         | last_line :: _ -> (i + 1, aux last_line line :: passed_lines))
       (0, [ lines.(0) ])
  |> snd |> List.rev |> Array.of_list

let count_splits beamed =
  beamed |> Utils.transpose_string_array
  |> Array.map (Utils.find_occurances "|^")
  |> Array.to_list |> List.flatten |> List.length

let count_timelines beamed =
  (* number, pos *)
  let initial_beam = (1, String.index beamed.(0) 'S') in
  Array.sub beamed 1 (Array.length beamed - 1)
  |> Array.fold_left
       (fun beams line ->
         let splitters = Utils.find_occurances "^" line in
         let new_beams =
           List.map
             (fun (number, pos) ->
               match Utils.list_contains pos splitters with
               | true -> [ (number, pos - 1); (number, pos + 1) ]
               | false -> [ (number, pos) ])
             beams
           |> List.flatten
           |> List.sort (fun (_, a) (_, b) -> a - b)
           |> List.fold_left
                (fun acc (num, pos) ->
                  match acc with
                  | [] -> [ (num, pos) ]
                  | (ln, lp) :: tl when pos = lp -> (ln + num, pos) :: tl
                  | beams -> (num, pos) :: beams)
                []
         in
         new_beams)
       [ initial_beam ]
  |> List.map (fun (number, _) -> number)
  |> List.fold_left ( + ) 0

let run file =
  let lines = Utils.readlines file |> Utils.drop_empty_lines |> Array.of_list in
  let beamed = run_beams lines in
  (* Array.iter print_endline beamed; *)
  count_splits beamed |> Utils.print 6;
  count_timelines beamed |> Utils.print 6;
  ignore lines
