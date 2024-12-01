#mod_use "utils.ml";;
open Utils;;

#load "str.cma";;

let example_input = [3;4;2;1;3;3],[4;3;5;3;9;3];;

List.sort (-) [3;4;2;1;3;3];;

let solve_part1 ((xs, ys)) =
  List.fold_left2
    (fun acc a b -> (acc + (Int.abs(b - a))))
    0
    (List.sort (-) xs)
    (List.sort (-) ys);;

solve_part1 example_input;;

let line_regex = Str.regexp "^\\([0-9]+\\) +\\([0-9]+\\)"

let parse_line s =
  try
    let _ = Str.search_forward line_regex s 0 in
    int_of_string(Str.matched_group 1 s),int_of_string(Str.matched_group 2 s)
  with
    Not_found -> raise (Invalid_argument s);;


let parse lines =
  lines
  |> List.map parse_line
  |> List.fold_left (fun acc ns ->
         let (xs, ys) = acc
         and (x, y) = ns in
         x::xs, y::ys) ([],[]);;

read_lines "../../data/day01.input"
|> parse
|> solve_part1;;

let frequencies xs =
  let rec loop acc current count xs =
    match (current, xs) with
      _, [] -> acc
    | None, b::tl -> loop acc (Some b) 1 tl
    | Some a, b::tl when a == b -> loop acc current (count + 1) tl
    | Some a, _ -> loop ((a,count)::acc) None 0 xs in
  loop [] None 0 (List.sort (-) xs);;

List.assoc_opt 5 (frequencies [1;2;3;1;3;4]);;

let solve_part2 ((xs, ys)) =
  let fs = frequencies ys in
  List.fold_left
    (fun acc a ->
      acc + (match (List.assoc_opt a fs) with
         None -> 0
       | Some n -> n * a))
    0 xs;;

solve_part2 example_input;;

read_lines "../../data/day01.input"
|> parse
|> solve_part2;;
