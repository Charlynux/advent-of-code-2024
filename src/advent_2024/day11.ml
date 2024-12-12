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

(*
RECHERCHES INFRUCTUEUSES AUTOUR DE LA MEMOIZATION...
 *)

type individual_blinks = int * int;;
type work = int * int list;;

let split ((blinks, stones) : work) : individual_blinks list =
  List.map (fun stone -> (blinks, stone)) stones;;

(*
  https://cs3110.GitHub.io/textbook/chapters/ds/memoization.html#memoization-using-higher-order-functions
 *)
let memoed_blink_init n =
  let h = Hashtbl.create n in
  let rec memoed ((blinks, ns) as w) =
    try Hashtbl.find h w
    with Not_found ->
      match (blinks) with
      | 0 -> ns
      | 1 -> List.concat_map blink ns
      | x -> let result =
               memoed (x - 1, memoed (1, ns)) in
          Hashtbl.add h w result;
          result in
  memoed;;

let rec run (blink_fn : individual_blinks -> work) = function
  | [] -> []
  | todos ->
     let (dones, remaining) =
       todos
       |> List.concat_map (fun w -> split w)
       |> List.map blink_fn
       |> List.partition (fun w -> (fst w) == 0) in
     (List.concat_map snd dones)
     @(run blink_fn remaining);;

let fn = (memoed_blink_init 10000) in
    fn (40, example_input);;
