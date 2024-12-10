#mod_use "utils.ml";;
open Utils;;

#load "str.cma";;

type point = int * int;;
module Point = struct
  type t = point
  let compare (x0,y0) (x1,y1) =
    match Stdlib.compare x0 x1 with
      0 -> Stdlib.compare y0 y1
    | c -> c
end
module PointsMap = Map.Make(Point);;
module PointsSet = Set.Make(Point);;

let update_acc_fn acc pos c =
  if (c == '.') then acc
  else PointsMap.add pos ((int_of_char c) - 48) acc;;

let parse_line map y line =
  let rec loop acc x s =
    if (String.length s = 0) then
      acc
    else
      loop
        (update_acc_fn acc (x, y) (String.get s 0))
        (x + 1)
        (String.sub s 1 ((String.length s) - 1)) in
  loop map 0 line;;

let parse_input lines =
  let rec loop acc y lines =
    match lines with
      [] -> acc
    | line :: rest -> loop
                        (parse_line acc y line)
                        (y + 1)
                        rest in
  let map = loop PointsMap.empty 0 lines in
  map;;

let example_map = parse_input (read_lines "../../data/day10-example.input");;
let example0_map = parse_input (read_lines "../../data/day10-example0.input");;
let example1_map = parse_input (read_lines "../../data/day10-example1.input");;

let move (x,y) (x',y') = (x + x', y + y');;

let starting_points map =
  map
  |> PointsMap.bindings
  |> List.filter (fun (_, c) -> c == 0)
  |> List.map
       (fun x -> PointsSet.of_list [fst x]);;

starting_points example_map;;
starting_points example1_map;;

let directions = [(0,1);(0,-1);(1,0);(-1,0)];;

let is_correct map n p =
  match (PointsMap.find_opt p map) with
    None -> false
  | Some n' -> n' == n;;

let step map n trailheads =
  List.map (fun positions ->
         positions
         |> PointsSet.to_list
         |> List.concat_map (fun p -> List.map (move p) directions)
         |> List.filter (fun p -> is_correct map n p)
         |> PointsSet.of_list
       ) trailheads;;

let found_trailheads map =
  let rec loop n currents =
    let n' = n + 1 in
    let nexts = step map n' currents in
    if (n' == 9) then
      nexts
    else
      loop n' nexts in
  loop 0 (starting_points map);;

let solve_part1 map =
  map
  |> found_trailheads
  |> List.map PointsSet.cardinal
  |> list_sum;;

solve_part1 example_map;;
solve_part1 example0_map;;
solve_part1 (parse_input (read_lines "../../data/day10.input"));;
