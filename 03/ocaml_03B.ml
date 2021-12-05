open Core

type rating = Oxygen | CO2

let solve_p2 inp =
 let rating_bools r =
  match r with
   | Oxygen -> fun x -> x
   | CO2 -> fun x -> not x 
 in
  
 let transpose_col bslist col = List.map bslist ~f:(fun bs -> (String.get bs col)) in

 let rec count_bits bs acc =
  match bs with
  | hd::tl -> count_bits tl (acc + Char.get_digit_exn hd)
  | _ -> acc
 in
 
 let check_bit_criteria r bslist col =
  let ones = count_bits (transpose_col bslist col) 0 in
  if ((rating_bools r) (ones >= (List.length bslist - ones))) then '1' else '0'
 in

 let rec filter_lines bslist r col =
  match (List.length bslist) with
  | 0 -> 0
  | 1 -> int_of_string ("0b" ^ (List.hd_exn bslist))
  | _ -> filter_lines (List.filter bslist ~f:(fun bs -> phys_equal (String.get bs col) (check_bit_criteria r bslist col))) r (col + 1)
 in
 
 (filter_lines inp Oxygen 0) * (filter_lines inp CO2 0)
 
 
let () = print_endline (string_of_int (solve_p2 (In_channel.read_lines ("input"))))
