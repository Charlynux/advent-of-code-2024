#mod_use "utils.ml";;
open Utils;;

#load "str.cma";;

type order_rule = int * int;;

let parse_rule (s: string) : order_rule =
  s
  |> String.split_on_char '|'
  |> List.map int_of_string
  |> (function [a;b] -> (a,b) | _ -> raise Not_found);;

let parse_update (s: string) =
  s
  |> String.split_on_char ','
  |> List.map int_of_string;;

let parse_input lines =
  let rec loop_rules rules lines =
    match (lines) with
    | ""::tl -> (tl, rules)
    | line::tl -> loop_rules ((parse_rule line)::rules) tl
  in
  let rec loop_updates updates lines =
    match (lines) with
    | [] -> updates
    | line::tl -> loop_updates ((parse_update line)::updates) tl
  in
  let (second_part, rules) = loop_rules [] lines in
  let updates = loop_updates [] second_part in
  (rules, updates);;

parse_input (read_lines "../../data/day05-example.input");;

let check_number rules n rest =
  List.for_all
    (fun m -> not (List.exists (fun r -> r = (m, n)) rules))
    rest;;

check_number [(75,43)] 43 [12;75];;

let is_valid rules update =
  let rec loop ns =
    match (ns) with
      [] -> true
    | [n] -> true
    | n::tl when (check_number rules n tl) -> loop tl
    | _::tl -> false
  in loop update;;

let solve_part1 input =
  let (rules, updates) = parse_input (read_lines input) in
    updates
    |> List.filter (is_valid rules)
    |> List.map (fun update -> List.nth update ((List.length update) / 2))
    |> list_sum;;


solve_part1 "../../data/day05-example.input";;
solve_part1 "../../data/day05.input";;
