#mod_use "utils.ml";;
open Utils;;

#load "str.cma";;

type direction = Up | Right | Left | Down;;

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
  (match (direction) with
     Up -> (0, -1)
   | Down -> (0, 1)
   | Right -> (1, 0)
   | Left -> (-1, 0));;

let turn_right ({pos; direction} : guard) =
  {pos = pos;
   direction = (match (direction) with
                  Up -> Right
                | Down -> Left
                | Right -> Down
                | Left -> Up)};;

let exit_from_map ({guard; obstacles; dimensions} : input) =
  let rec loop points guard =
    if (is_out dimensions guard.pos) then
      points
    else
      loop
        (PointsSet.add guard.pos points)
        (let next_pos = move_forward(guard) in
          if (PointsSet.mem next_pos obstacles) then
            turn_right(guard)
          else
            {pos = next_pos; direction = guard.direction})
  in
  loop PointsSet.empty guard;;

let solve_part1 lines =
  lines
  |> parse_input
  |> exit_from_map
  |> PointsSet.cardinal;;


solve_part1 (read_lines "../../data/day06-example.input");;
solve_part1 (read_lines "../../data/day06.input");;
