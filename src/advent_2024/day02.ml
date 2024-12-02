#mod_use "utils.ml";;
open Utils;;

#load "str.cma";;

let read_numbers s = s |> String.split_on_char ' ' |> List.map int_of_string;;

let diffs ns =
  let rec loop acc ns =
    match (ns) with
      a::b::tl -> loop ((b-a)::acc) (b::tl)
    | _ -> acc in
  loop [] ns;;

let safe_predicate ns =
  let h = List.hd ns in
  let pred = if (h > 0) then (fun n -> n > 0) else (fun n -> n < 0) in
  List.for_all (fun n -> (pred n) && (1 <= Int.abs(n) && Int.abs(n) <= 3)) ns;;

let is_safe ns = ns |> diffs |> safe_predicate;;

let example_input =
  ["7 6 4 2 1"; "1 2 7 8 9";"9 7 6 2 1"; "1 3 2 4 5";"8 6 4 4 1";"1 3 6 7 9"];;

let solve_part1 input =
  input
|> List.map read_numbers
|> List.filter is_safe
|> List.length;;

solve_part1 (read_lines "../../data/day02.input");;
