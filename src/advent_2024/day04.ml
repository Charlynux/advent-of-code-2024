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

let parse_line map y line =
  let rec loop acc x s =
    if (String.length s = 0) then
      acc
    else
      loop
        (PointsMap.add (x, y) (String.get s 0) acc)
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

type movement = int * int -> int * int;;

type direction = Right | Left | Bottom | Up;;

let example_map = parse_input (read_lines "../../data/day04-example.input");;

PointsMap.find_opt (0,0) example_map;;

(** Inutile pour ce problème finalement *)
let find_maxs map =
  let points = map |> PointsMap.bindings |> List.map fst in
  (points |> List.map fst |> List.sort (fun a b -> b - a) |> List.hd,
   points |> List.map snd |> List.sort (fun a b -> b - a) |> List.hd);;
find_maxs example_map;;

let directions = [(0,1);(0,-1);(1,0);(1,-1);(1,1);(-1,0);(-1,1);(-1,-1)];;

let move (x,y) (x',y') = (x + x', y + y');;

(** On sort tous les 'X' qui représentent nos points de départ. *)
let starting_points map =
  map
  |> PointsMap.bindings
  |> List.filter (fun (_, c) -> c == 'X')
(** On assigne à chacun toutes les directions à suivre *)
  |> List.concat_map (fun (p, c) -> List.map (fun d -> (p,c,d)) directions)
;;

let expand (p, c, d) =
  let next_char =
    match (c) with
     |'X' -> 'M'
     |'M' -> 'A'
     |'A' -> 'S'
     |_ -> raise (Invalid_argument (String.make 1 c)) in
  ((move p d), next_char, d);;

expand ((0,0), 'X', (1, 1));;

let is_correct map (p, c, d) =
  match (PointsMap.find_opt p map) with
    None -> false
  | Some c' -> c' == c;;

starting_points example_map;;

PointsMap.find_opt (0,4) example_map;;

example_map
|> starting_points
|> List.map expand
|> List.filter (is_correct example_map);;

let solve_part1 map =
  let rec loop dones currents =
    if (currents == []) then
      dones
    else
      let corrects = List.filter (is_correct map) currents in
      let (new_dones, futures) = List.partition (fun (p, c, d) -> c == 'S') corrects in
      loop (new_dones@dones) (List.map expand futures) in
  loop [] (starting_points map);;

solve_part1 example_map |> List.length;;

read_lines "../../data/day04.input" |> parse_input |> solve_part1 |> List.length;;

type diagonale = TopDown | DownTop;;

let diagonale_to_direction = function
  | TopDown -> ((-1,1),(1,-1))
  | DownTop -> ((-1,-1),(1,1));;

let is_mas map a_point diag =
  let (p, p') = diagonale_to_direction diag in
  match (PointsMap.find_opt (move a_point p) map,
         PointsMap.find_opt (move a_point p') map) with
    (Some 'M', Some 'S') | (Some 'S', Some 'M') -> true
    | _ -> false;;

let is_xmas map a_point =
  is_mas map a_point TopDown
  && is_mas map a_point DownTop;;

let a_points map =
  map
  |> PointsMap.bindings
  |> List.filter (fun (_, c) -> c == 'A')
  |> List.map fst;;

a_points example_map;;

let solve_part2 map =
  map
  |> a_points
  |> List.filter (is_xmas map)
  |> List.length;;

solve_part2 example_map;;

read_lines "../../data/day04.input" |> parse_input |> solve_part2 ;;
