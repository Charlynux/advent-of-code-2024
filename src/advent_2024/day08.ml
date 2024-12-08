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
  loop
    []
    0
    lines;;

let example_input = parse_input (read_lines "../../data/day08-example.input");;
parse_input (read_lines "../../data/day08.input");;
