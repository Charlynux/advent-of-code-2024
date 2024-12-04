#mod_use "utils.ml";;
open Utils;;

#load "str.cma";;

let example_input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";;

let mul_regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))";;

let find_mults s =
  let rec loop acc n =
    try
      let m = Str.search_forward mul_regex s n in
      let a = int_of_string(Str.matched_group 1 s)
      and b = int_of_string(Str.matched_group 2 s) in
      loop ((a, b)::acc) (m + 1)
    With
      Not_found -> acc in
  loop [] 0;;

find_mults example_input;;

let mult (a,b) = a * b;;

let calculate_line s = s |> find_mults |> List.map mult |> list_sum;;

read_lines ("../../data/day03.input")
|> List.map calculate_line
|> list_sum;;

let example_input2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";;

let part2_regex = Str.regexp "\\(don't()\\|do()\\|mul(\\([0-9]+\\),\\([0-9]+\\))\\)";;

type op =
  | Do
  | Dont
  | Mult of int * int;;

let find_ops s =
  let rec loop n =
    try
      let next = (Str.search_forward part2_regex s n) + 1 in
      let found = Str.matched_group 1 s in
      match (found) with
        "do()" -> Do::(loop next)
      | "don't()" -> Dont::(loop next)
      | _ ->
         let a = int_of_string(Str.matched_group 2 s)
         and b = int_of_string(Str.matched_group 3 s) in
         Mult(a,b)::(loop next)
    with
      Not_found -> [] in
  loop 0;;

find_ops example_input2;;

let rec aggregate_ops enabled xs =
  match (xs) with
    [] -> []
  | Do::tl -> aggregate_ops true tl
  | Dont::tl -> aggregate_ops false tl
  | Mult(a,b)::tl when enabled -> (a*b)::(aggregate_ops enabled tl)
  | Mult(a,b)::tl -> aggregate_ops enabled tl;;

aggregate_ops true (find_ops example_input2);;

read_lines ("../../data/day03.input")
|> List.concat_map find_ops
|> (aggregate_ops true)
|> list_sum;;
(**
   69247082 - too high
   Je traitais les lignes une à une, alors que le statut "enabled" doit être conservé de ligne en ligne.

   63013756 - Correct
 *)
