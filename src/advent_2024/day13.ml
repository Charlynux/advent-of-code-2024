#mod_use "utils.ml";;
open Utils;;

#load "str.cma";;

let button_regexp = Str.regexp {|Button [AB]: X\+\([0-9]+\), Y\+\([0-9]+\)|};;
let prize_regexp = Str.regexp {|Prize: X=\([0-9]+\), Y=\([0-9]+\)|};;

let read_pair r s =
  let _ = Str.search_forward r s 0 in
  (int_of_string (Str.matched_group 1 s),
   int_of_string (Str.matched_group 2 s));;

read_pair button_regexp "Button A: X+94, Y+34";;
read_pair prize_regexp "Prize: X=8400, Y=5400";;

let parse_input lines =
  let parse_group [prize; b; a] =
    (read_pair button_regexp a,
     read_pair button_regexp b,
     read_pair prize_regexp prize) in
  let rec loop currents = function
    | [] -> [parse_group currents]
    | hd :: tl when hd = "" ->
       (parse_group currents)::(loop [] tl)
    | hd :: tl -> loop (hd::currents) tl in
  loop [] lines;;

parse_input
  ["Button A: X+94, Y+34";
   "Button B: X+22, Y+67";
   "Prize: X=8400, Y=5400"];;

type point = int * int;;
module Point = struct
  type t = point
  let compare (x0,y0) (x1,y1) =
    match Stdlib.compare x0 x1 with
      0 -> Stdlib.compare y0 y1
    | c -> c
end

let move (x,y) (x',y') = (x + x', y + y');;
let mult n (x, y) = (x * n, y * n);;

(0, 0) |> move (mult 80 (94,34)) |> move (mult 40 (22,67));;

let find_movement prize a b =
    let rec loop a_pushes =
      let rec b_loop b_pushes =
        if (b_pushes > 100) then
          None
        else
          match ((0, 0) |> move (mult a_pushes a) |> move (mult b_pushes b)) with
          | (x, y) when x = (fst prize) && y = (snd prize) -> Some (a_pushes, b_pushes)
          | (x, y) when x > (fst prize) || y > (snd prize) -> None
          | _ -> b_loop (b_pushes + 1) in
      if (a_pushes > 100) then
        None
      else
        match (b_loop 0) with
        | None -> loop (a_pushes + 1)
        | res -> res in
    loop 0;;

let solve_part1 file =
  parse_input (read_lines file)
  |> List.map (fun (a,b,p) ->
         match (find_movement p a b) with
           None -> 0
         | Some (a, b) -> (a * 3) + b)
  |> list_sum;;

solve_part1 "../../data/day13-example.input";;
solve_part1 "../../data/day13.input";;
