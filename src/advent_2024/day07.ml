#mod_use "utils.ml";;
open Utils;;

#load "str.cma";;

let is_solvable target numbers =
  let rec loop current numbers =
    if (current > target) then
      (*
        Vu qu'on n'utilise que la multiplication et l'addition de positifs,
        on ne peut pas faire baisser le nombre obtenu.
       *)
      false
    else
      match (numbers) with
        [] -> current == target
      | n::tl ->
         loop (current * n) tl || loop (current + n) tl
  in
  loop (List.hd numbers) (List.tl numbers);;

is_solvable 190 [10; 19];;
is_solvable 3267 [81; 40; 27];;
is_solvable 7290 [6; 8; 6;15];;

let parse_line line =
  let [target; numbers] = String.split_on_char ':' line in
  (int_of_string target,
   (numbers |> String.trim
    |> String.split_on_char ' '
    |> List.map int_of_string));;

parse_line "7290: 6 8 6 15";;

let solve_part1 lines =
  lines
  |> List.map parse_line
  |> List.filter (fun (target, numbers) -> is_solvable target numbers)
  |> List.map fst
  |> list_sum;;

solve_part1 (read_lines "../../data/day07-example.input");;
solve_part1 (read_lines "../../data/day07.input");;
