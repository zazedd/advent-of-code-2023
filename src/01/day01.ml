let is_int c = Char.code c >= 48 && Char.code c <= 57

let rec bs i j l =
  match (l.[i], l.[j]) with
  | a, b when is_int a && is_int b ->
      ((Char.code a - Char.code '0') * 10) + (Char.code b - Char.code '0')
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
    List.assoc_opt n_str number_map
    |> Option.value ~default:(Char.code n_str.[0] - Char.code '0')
  in
  let _ = Str.search_forward regex s 0 in
  let n1 = get_num s in
  let _ = Str.search_backward regex s (String.length s - 1) in
  let n2 = get_num s in
  (n1 * 10) + n2

let part2 l = List.fold_left (fun acc a -> search a + acc) 0 l
let l = Utils.read_input "src/01/input.txt"
let () = part1 l |> string_of_int |> print_endline
let () = part2 l |> string_of_int |> print_endline
