#mod_use "utils.ml";;
open Utils;;

#load "str.cma";;

let concatenation a b =
  int_of_string
    (String.cat (string_of_int a) (string_of_int b));;

type operator =
  | Add
  | Multiplication
  | Concatenation;;

let calculate operator a b =
  match (operator) with
    Add -> a + b
  | Multiplication -> a * b
  | Concatenation -> concatenation a b;;

let is_solvable operators target numbers =
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
         List.exists
           (fun op -> loop (calculate op current n) tl)
           operators
  in
  loop (List.hd numbers) (List.tl numbers);;

is_solvable [Add;Multiplication] 190 [10; 19];;
is_solvable [Add;Multiplication] 3267 [81; 40; 27];;
is_solvable [Add;Multiplication] 7290 [6; 8; 6;15];;

let parse_line line =
  let [target; numbers] = String.split_on_char ':' line in
  (int_of_string target,
   (numbers |> String.trim
    |> String.split_on_char ' '
    |> List.map int_of_string));;

parse_line "7290: 6 8 6 15";;

let solve_part operators lines =
  let test_fn = is_solvable operators in
  lines
  |> List.map parse_line
  |> List.filter (fun (target, numbers) -> test_fn target numbers)
  |> List.map fst
  |> list_sum;;

let solve_part1 = solve_part [Add;Multiplication];;

solve_part1 (read_lines "../../data/day07-example.input");;
solve_part1 (read_lines "../../data/day07.input");;


let solve_part2 = solve_part [Add;Multiplication;Concatenation];;

solve_part2 (read_lines "../../data/day07-example.input");;
solve_part2 (read_lines "../../data/day07.input");;
