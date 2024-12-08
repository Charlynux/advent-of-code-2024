#mod_use "utils.ml";;
open Utils;;

#load "str.cma";;

let update_assoc l k v =
  match (List.assoc_opt k l) with
    None -> (k,[v])::l
  | Some vs -> (k,v::vs)
               ::(List.remove_assoc k l);;

let parse_line acc y line =
  let rec loop acc x s =
    if (String.length s = 0) then
      acc
    else
      let c = String.get s 0 in
      let new_acc =  if (c == '.') then
                       acc
                     else
                       update_assoc acc c (x,y) in
      loop
        new_acc
        (x + 1)
        (String.sub s 1 ((String.length s) - 1))
     in
  loop acc 0 line;;

let parse_input lines =
  let rec loop acc y lines =
    match lines with
      [] -> acc
    | line :: rest -> loop
                        (parse_line acc y line)
                        (y + 1)
                        rest in
  ((List.length lines, String.length (List.hd lines)),
   loop [] 0 lines);;

let example_input = parse_input (read_lines "../../data/day08-example.input");;
parse_input (read_lines "../../data/day08.input");;

let find_antinodes (x, y) (x', y') =
  let offset_x = x' - x
  and offset_y = y' - y in
  [
    ((x - offset_x),(y - offset_y));
    ((x' + offset_x),(y' + offset_y))
  ];;

find_antinodes (5, 5) (4, 3);;
(* (3, 1) (6, 7)*)

let rec pairing_antennas = function
    [] -> []
  | [a] -> []
  | a :: tl -> (List.concat_map (find_antinodes a) tl)
               @(pairing_antennas tl);;

pairing_antennas [(5, 5);(4, 3);(8,4)];;

let is_inbound (max_x, max_y) (x, y) =
 (x >= 0) && (x < max_x) && (y >= 0) && (y < max_y);;

let compare_points (x0,y0) (x1,y1) =
  match Stdlib.compare x0 x1 with
    0 -> Stdlib.compare y0 y1
  | c -> c;;

let solve_part1 (dimensions, antennas) =
  antennas
  |> List.map snd
  |> List.concat_map pairing_antennas
  |> List.filter (is_inbound dimensions)
  |> List.sort_uniq compare_points;;

(** Comparaison des résultats de l'exemple *)
"../../data/day08-example-solution.input"
|> read_lines |> parse_input |> snd |> (List.assoc '#')
|> (List.sort compare_points);;

solve_part1 example_input |> List.length;;

parse_input (read_lines "../../data/day08.input")
|> solve_part1
|> List.length;;
