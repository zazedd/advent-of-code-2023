let get_id s = List.nth (String.split_on_char ' ' s) 1 |> int_of_string

let check_valid s =
  let split_colors = String.split_on_char ',' s in
  List.fold_left
    (fun acc a ->
      match String.split_on_char ' ' a with
      | [ _; a; "red" ] -> int_of_string a <= 12 && acc
      | [ _; a; "green" ] -> int_of_string a <= 13 && acc
      | [ _; a; "blue" ] -> int_of_string a <= 14 && acc
      | _ -> assert false)
    true split_colors

let rec part1 acc = function
  | [] -> acc
  | x :: xs ->
      let id_str, draws_str =
        let tmp = String.split_on_char ':' x in
        (List.hd tmp, List.tl tmp |> List.hd)
      in
      let id = get_id id_str in
      let draws = String.split_on_char ';' draws_str in
      let rec check_all_draws acc = function
        | [] -> acc
        | y :: ys -> check_all_draws (check_valid y && acc) ys
      in
      if check_all_draws true draws then part1 (id + acc) xs else part1 acc xs

let get_nums s =
  let split =
    String.split_on_char ',' s |> List.map (String.split_on_char ' ')
  in
  let filter_color color lst =
    List.filter (fun a -> List.nth a 2 = color) lst
  in
  let reds = filter_color "red" split in
  let greens = filter_color "green" split in
  let blues = filter_color "blue" split in

  let get_max l =
    if List.length l = 0 then 0 else List.nth (List.hd l) 1 |> int_of_string
  in
  (get_max reds, get_max greens, get_max blues)

let rec part2 acc = function
  | [] -> acc
  | x :: xs ->
      let draws_str =
        let tmp = String.split_on_char ':' x in
        List.tl tmp |> List.hd
      in
      let draws = String.split_on_char ';' draws_str in
      let rec get_max_nums r_max g_max b_max = function
        | [] -> r_max * g_max * b_max
        | y :: ys ->
            let r, g, b = get_nums y in
            get_max_nums (max r r_max) (max g g_max) (max b b_max) ys
      in
      part2 (acc + get_max_nums 0 0 0 draws) xs

let l = Utils.read_input "src/02/input.txt"
let () = part1 0 l |> string_of_int |> print_endline
let () = part2 0 l |> string_of_int |> print_endline
