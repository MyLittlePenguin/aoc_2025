exception ElfPanic

let split_by_whitespace line =
  let len = String.length line in
  let rec aux acc start i =
    let ch = String.get line i in
    let next = i + 1 in
    match start with
    | None when next >= len -> acc
    | Some start when next >= len ->
        (String.sub line start (next - start) |> String.trim) :: acc
    | None when ch = ' ' -> aux acc None next
    | None -> aux acc (Some i) next
    | Some start when ch = ' ' ->
        let word = String.sub line start (i - start) in
        aux (word :: acc) None next
    | start -> aux acc start next
  in
  aux [] None 0 |> List.rev |> Array.of_list

let run file =
  let lines =
    Utils.readlines file |> Utils.drop_empty_lines
    |> List.map split_by_whitespace
    |> List.rev |> Array.of_list
  in
  let cols = Utils.range 0 (Array.length lines.(0) - 1) in
  let rows = Utils.range 1 (Array.length lines - 1) in
  let results =
    List.map
      (fun col ->
        match lines.(0).(col) with
        | "+" ->
            List.map (fun row -> lines.(row).(col) |> int_of_string) rows
            |> Utils.sum_int
        | "*" ->
            List.map (fun row -> lines.(row).(col) |> int_of_string) rows
            |> Utils.product_int
        | _ -> raise ElfPanic)
      cols
  in
  results |> Utils.sum_int |> Utils.print 6;

  let lines = Utils.readlines file |> Utils.drop_empty_lines |> Array.of_list in
  let last_line = Array.length lines - 1 in
  let line_length = String.length lines.(0) in
  let transposed_no_op =
    Array.init line_length (fun col ->
        String.init last_line (fun row -> String.get lines.(row) col))
  in
  let operators = lines.(last_line) in
  let rec aux last_op sum acc i =
    let next = i + 1 in
    if i >= line_length then acc + sum
    else if String.trim transposed_no_op.(i) |> String.length = 0 then
      aux last_op (acc + sum) 0 next
    else
      let column = transposed_no_op.(i) |> String.trim in
      let number = int_of_string column in
      match String.get operators i with
      | '*' -> aux '*' (acc + sum) number next
      | '+' -> aux '+' (acc + sum) number next
      | _ ->
          aux last_op sum
            (number
            |> (if last_op = '+' then ( + ) else ( * )) acc)
            next
  in
  aux ' ' 0 0 0 |> Utils.print 6;
  ignore lines
