open Core

let inp = List.map ~f:int_of_string (In_channel.read_lines ("input"))

let solve_p1 inp =
 let rec f inp acc =
  match inp with
  | first::(second::_ as tail) -> f (tail) (acc + (if (first<second) then 1 else 0))
  | _ -> acc
 in
 f inp 0

let solve_p2 inp = 
 let rec f inp acc = 
  let firstfour lst = 
   match lst with
   | a::b::c::d::_ -> if (a+b+c < b+c+d) then 1 else 0
   | _ -> 0
  in
  match inp with
  | _::tail -> f (tail) (acc + (firstfour inp))
  | _ -> acc
 in
 f inp 0

let () = print_endline (String.concat ~sep:"\n" (List.map ~f:string_of_int [ (solve_p1 inp); (solve_p2 inp);]))

