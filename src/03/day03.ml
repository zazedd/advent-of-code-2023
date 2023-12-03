type s = { i : int; j : int; id : char }
type n = { i : int; j : int; length : int; num : int }

let neigh (s : s) (n : n) =
  let col_east = s.j >= n.j - 1 in
  let col_west = s.j <= n.j + n.length in
  let east = s.i >= n.i - 1 in
  let west = s.i <= n.i + 1 in
  east && west && col_east && col_west

let part1 nums syms =
  List.filter_map
    (fun a ->
      if List.exists (fun s -> neigh s a) syms then Some a.num else None)
    nums
  |> List.fold_left ( + ) 0

let line_to_numbers_and_symbols i line =
  let rec aux prev_num prev_num_len j nums syms input =
    let numbers =
      match prev_num_len with
      | 0 -> nums
      | _ ->
          { i; j = j - prev_num_len; length = prev_num_len; num = prev_num }
          :: nums
    in
    match input with
    | [] -> (numbers, syms)
    | c :: xs -> (
        match c with
        | '0' .. '9' as c ->
            aux
              ((prev_num * 10) + (Char.code c - Char.code '0'))
              (prev_num_len + 1) (j + 1) nums syms xs
        | _ ->
            let symbols =
              match c with '.' -> syms | _ -> { i; j; id = c } :: syms
            in
            aux 0 0 (j + 1) numbers symbols xs)
  in
  aux 0 0 0 [] [] line

let explode s = List.init (String.length s) (String.get s)

let part2 nums syms =
  List.filter_map
    (fun s ->
      match s.id with
      | '*' -> (
          List.filter (neigh s) nums |> fun a ->
          match a with
          | [ n1; n2 ] -> n1.num * n2.num |> Option.some
          | _ -> None)
      | _ -> None)
    syms
  |> List.fold_left ( + ) 0

let l = Utils.read_input "src/03/input.txt"

let pairs =
  List.mapi line_to_numbers_and_symbols (List.map (fun line -> explode line) l)

let nums = List.concat_map fst pairs
let syms = List.concat_map snd pairs
let () = part1 nums syms |> string_of_int |> print_endline
let () = part2 nums syms |> string_of_int |> print_endline
