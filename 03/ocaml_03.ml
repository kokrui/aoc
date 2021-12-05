open Core

let inp = In_channel.read_lines ("input")

let bs_width bs_list = String.length (List.hd_exn bs_list)

let bitstring_to_int bs = int_of_string ("0b" ^ bs)

let bit_arr = Array.create ~len:(bs_width inp) 0
 
let track_all_bits bs_list = 
 let count_track_bits bs =
  let incr i a = Array.set bit_arr i ((Array.get bit_arr i) + (Char.get_digit_exn a))
  in
  List.iteri ( String.to_list bs ) ~f:incr
 in
 List.iter bs_list ~f:count_track_bits

;; track_all_bits inp

let get_gamma =
 let buf = Buffer.create (bs_width inp) in
 let choose_bit i = if i > ((List.length inp)/2) then "1" else "0" in 
 let app_str i = Buffer.add_string buf (choose_bit i) in
 List.iter (Array.to_list bit_arr) ~f:app_str;
 Buffer.contents buf

(** non-pure implementation *)
let solve_p1 =
 let gamma = bitstring_to_int get_gamma in
 gamma * (gamma lxor (bitstring_to_int (String.concat ~sep:"" (List.init (bs_width inp) ~f:(const "1")))))

let () = print_endline (string_of_int solve_p1)
