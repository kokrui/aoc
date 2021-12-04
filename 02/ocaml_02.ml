open Core
open Core.String

let inp = In_channel.read_lines ("input")

let parse_line lne = 
 match split ~on:' ' lne with
 | [ action; num; ] -> (action, int_of_string (num))
 | _ -> failwith "y u parsing empty line bro" 

let resolve_line_p1 lne dimension =
 match ((parse_line lne), dimension) with
 | ("forward", x), "hpos" -> 1*x
 | ("up", x), "depth" -> -1*x
 | ("down", x), "depth" -> 1*x
 | _ -> 0

let solve_p1 inp =
 let rec f inp hpos depth =
  match inp with 
  | hd::tl -> f (tl) (hpos + resolve_line_p1 hd "hpos") (depth + resolve_line_p1 hd "depth")
  | _ -> hpos * depth
 in
 f inp 0 0

let resolve_aim_change_p2 lne =
 match parse_line lne with
 | "up", x -> -1*x
 | "down", x -> 1*x
 | _ -> 0

let resolve_forward_p2 lne dimension aim =
 match ((parse_line lne), dimension) with
 | ("forward", x), "hpos" -> 1*x
 | ("forward", x), "depth" -> aim*x
 | _ -> 0

let solve_p2 inp =
 let rec f inp hpos depth aim =
  match inp with
  | hd::tl -> f (tl) (hpos + resolve_forward_p2 hd "hpos" aim) (depth + resolve_forward_p2 hd "depth" aim) (aim + resolve_aim_change_p2 hd)
  | _ -> hpos * depth
 in
 f inp 0 0 0

let () = print_endline (String.concat ~sep:"\n" (List.map ~f:string_of_int [ (solve_p1 inp); (solve_p2 inp); ]))

