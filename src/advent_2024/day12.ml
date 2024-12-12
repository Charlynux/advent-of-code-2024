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
  else PointsMap.add pos c acc;;

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

let example1_map = parse_input (read_lines "../../data/day12-example1.input");;
let example2_map = parse_input (read_lines "../../data/day12-example2.input");;
let example3_map = parse_input (read_lines "../../data/day12-example3.input");;

let is_correct map n p =
  match (PointsMap.find_opt p map) with
    None -> false
  | Some n' -> n' == n;;

let directions = [(0,1);(0,-1);(1,0);(-1,0)];;
let move (x,y) (x',y') = (x + x', y + y');;
let neighbors p = List.map (move p) directions;;

let find_next_region map =
  let (pos, c) = PointsMap.find_first (fun _ -> true) map in
  let rec loop points map = function
    | [] -> (map, points)
    | opens ->
       let points = PointsSet.union
                      points
                      (PointsSet.of_list opens)
       and map = List.fold_left
                   (fun acc p -> PointsMap.remove p acc) map opens in
       loop points map
         (opens
          |> List.concat_map neighbors
          |> List.filter (is_correct map c)) in
  loop PointsSet.empty map [pos];;

let rec find_regions map =
  if (PointsMap.is_empty map) then
    []
  else
    let (updated_map, region) = find_next_region map in
    region::(find_regions updated_map);;

find_regions example1_map;;
find_regions example2_map;;
find_regions example3_map;;
