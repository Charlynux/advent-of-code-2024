#mod_use "utils.ml";;
open Utils;;

#load "str.cma";;

let example_input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";;

let mul_regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))";;

let find_mults s =
  let rec loop acc n =
    try
      let m = Str.search_forward mul_regex s n in
      let a = int_of_string(Str.matched_group 1 s)
      and b = int_of_string(Str.matched_group 2 s) in
      loop ((a, b)::acc) (m + 1)
    with
      Not_found -> acc in
  loop [] 0;;

find_mults example_input;;

let mult (a,b) = a * b;;

let calculate_line s = s |> find_mults |> List.map mult |> list_sum;;

read_lines ("../../data/day03.input")
|> List.map calculate_line
|> list_sum;;
