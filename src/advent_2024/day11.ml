#mod_use "utils.ml";;
open Utils;;

#load "str.cma";;

let parse s = s |> String.split_on_char ' ' |> List.map int_of_string;;

let example_input = parse "125 17";;
let input = parse "6571 0 5851763 526746 23 69822 9 989";;

let blink = function
  | 0 -> [1]
  | n when (String.length (string_of_int n)) mod 2 == 0 ->
     let s = string_of_int n in
     let l = (String.length s) / 2 in
     List.map int_of_string
     [String.sub s 0 l; String.sub s l l]
  | n -> [n * 2024];;

blink 253000;;

let blink_n target stones =
  let rec loop n stones =
    if (n == target) then
      stones
    else
      loop (n + 1) (List.concat_map blink stones)
  in
  loop 0 stones;;

let solve_part1 s = s |> blink_n 25 |> List.length;;

solve_part1 example_input;;
solve_part1 input;;
s
