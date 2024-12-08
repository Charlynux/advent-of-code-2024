#mod_use "utils.ml";;
open Utils;;

#load "str.cma";;

type direction = Up | Right | Left | Down;;
let direction_offset = function
  | Up -> (0, -1)
  | Down -> (0, 1)
  | Right -> (1, 0)
  | Left -> (-1, 0);;

type point = int * int;;
module Point = struct
  type t = point
  let compare (x0,y0) (x1,y1) =
    match Stdlib.compare x0 x1 with
      0 -> Stdlib.compare y0 y1
    | c -> c
end;;
module PointsSet = Set.Make(Point);;

type guard = {
    pos : point;
    direction : direction;
  };;

module Guard = struct
  type t = guard
  let compare g0 g1 =
    match (Point.compare g0.pos g1.pos) with
      0 -> (Point.compare
              (direction_offset g0.direction)
              (direction_offset g1.direction))
    | c -> c
end;;
module GuardsSet = Set.Make(Guard);;

[{pos=(0,0);direction=Up};{pos=(0,0);direction=Down};{pos=(0,0);direction=Up}]
|> GuardsSet.of_list
|> GuardsSet.cardinal;;

type input = {
    obstacles : PointsSet.t;
    guard : guard;
    dimensions: point;
  };;

let parse_line acc y line =
  let rec loop acc x s =
    if (String.length s = 0) then
      acc
    else
      loop
      (match (String.get s 0) with
         '^' -> { guard = {pos = (x,y); direction = Up};
                  obstacles = acc.obstacles;
                  dimensions = acc.dimensions}
       | '#' -> { guard = acc.guard;
                  obstacles = PointsSet.add (x,y) acc.obstacles;
                  dimensions = acc.dimensions}
       | _ -> acc)
      (x + 1)
      (String.sub s 1 ((String.length s) - 1)) in
  loop acc 0 line;;

let parse_input lines =
  let rec loop acc y lines =
     match lines with
      [] -> acc
    | line :: rest -> loop
                        (parse_line acc y line)
                        (y + 1)
                        rest in
  loop
    {guard = {pos= (0,0); direction = Up};
     obstacles = PointsSet.empty;
     dimensions = (List.length lines, String.length (List.hd lines))}
    0
    lines;;

parse_input (read_lines "../../data/day06-example.input");;

let is_out (max_x, max_y) (x, y) =
  x < 0 || x >= max_x || y < 0 || y >= max_y;;

let move (x,y) (x',y') = (x+x',y+y');;

let move_forward ({pos;direction} : guard) =
  move
    pos
    (direction_offset direction);;

let turn_right ({pos; direction} : guard) =
  {pos = pos;
   direction = (match (direction) with
                  Up -> Right
                | Down -> Left
                | Right -> Down
                | Left -> Up)};;

let guard_move obstacles guard =
  let next_pos = move_forward guard in
  if (PointsSet.mem next_pos obstacles) then
    turn_right(guard)
  else
    {pos = next_pos; direction = guard.direction};;

type move_result =
  | Exit of GuardsSet.t
  | LoopFound;;

let move_guard_to_end dimensions obstacles guard =
  let rec loop points guard =
    match (is_out dimensions guard.pos, GuardsSet.mem guard points) with
      (true, _) -> Exit points
    | (_, true) -> LoopFound
    | _ -> loop
        (GuardsSet.add guard points)
        (guard_move obstacles guard)
  in
  loop GuardsSet.empty guard;;

let exit_from_map ({guard; obstacles; dimensions} : input) =
  match (move_guard_to_end dimensions obstacles guard) with
    Exit points -> points
  | _ -> raise Not_found;;

let solve_part1 lines =
  lines
  |> parse_input
  |> exit_from_map
  |> GuardsSet.to_list
  |> List.map (fun g -> g.pos)
  |> List.sort_uniq Point.compare
  |> List.length;;

solve_part1 (read_lines "../../data/day06-example.input");;
solve_part1 (read_lines "../../data/day06.input");;

(* Test Loop detection *)
let {guard; obstacles; dimensions} = parse_input (read_lines "../../data/day06-example.input") in
    move_guard_to_end
      dimensions
      (PointsSet.add (3,6) obstacles)
      guard
;;
