let read_input path =
  let ic = open_in path in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []

let is_int c = Char.code c >= 48 && Char.code c <= 57

let combine c1 c2 =
  ((Char.code c1 - Char.code '0') * 10) + (Char.code c2 - Char.code '0')

let l = read_input "input.txt"

let rec bs i j l =
  match (l.[i], l.[j]) with
  | a, b when is_int a && is_int b -> combine a b
  | a, _ when is_int a -> bs i (j - 1) l
  | _, b when is_int b -> bs (i + 1) j l
  | _, _ -> bs (i + 1) (j - 1) l

let part1 l =
  List.fold_left (fun acc a -> bs 0 (String.length a - 1) a + acc) 0 l

let number_map =
  [
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
  ]

let regex =
  Str.regexp
    "\\(one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine\\|[0-9]\\)"

let search s =
  let get_num s =
    let n_str = Str.matched_string s in
    let n =
      match List.assoc_opt n_str number_map with
      | Some num -> num
      | None -> Char.code n_str.[0] - Char.code '0'
    in
    n
  in
  let _ = Str.search_forward regex s 0 in
  let n1 = get_num s in
  let _ = Str.search_backward regex s (String.length s - 1) in
  let n2 = get_num s in
  (n1 * 10) + n2

let part2 l = List.fold_left (fun acc a -> search a + acc) 0 l
let () = part1 l |> string_of_int |> print_endline
let () = part2 l |> string_of_int |> print_endline
