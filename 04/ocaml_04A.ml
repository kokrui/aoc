open Core

let inp = In_channel.read_lines ("input")

type board_state = (int * bool) array array
type inp_tup = {
 nums : int list;
 boards : board_state list;
}

let parse_inp inp = 
 match (List.group ~break:(fun _ -> String.is_empty) inp) with
 | [nms]::bds -> 
  let nlist = List.map ~f:int_of_string (String.split nms ~on:',') in
  let parse_row row =   
   let remove_empty r = List.filter ~f:(fun x -> not (String.is_empty x)) (String.split ~on:' ' r) in
   let to_ints r = List.map ~f:int_of_string (remove_empty r) in
   Array.of_list (List.map ~f:(fun x -> (x, false)) (to_ints row))
  in
  let parse_board b = Array.of_list (List.map ~f:parse_row (List.tl_exn b)) in
  let blist =
   match List.length bds with
   | 0 -> failwith "huh no boards? prolly sth wrong with List.group above bro"
   | l -> print_endline (string_of_int (l+69));print_endline (List.nth_exn (List.hd_exn bds) 2);List.map ~f:parse_board bds;
  in
  { 
   nums = nlist;
   boards = blist;
  }
 | _ -> failwith "no shot ur here"

let visit_num n b = Array.map b ~f:(fun r -> Array.map r ~f:(fun (x, visited) -> (x, visited || x=n)))

let check_lines b =
 let check_direction brd = Array.exists brd ~f:(fun r -> (Array.for_all ~f:(snd) r)) in
 (check_direction b || check_direction (Array.transpose_exn b))

let win_output n b =
 (*String.concat ~sep:" " (List.map ~f:(fun x -> string_of_int (fst x)) (Array.to_list (Array.get n 0)))*)
 (*n*)
 n*(Array.fold b ~init:0 ~f:(fun rowsum r -> Array.fold r ~init:rowsum ~f:(fun cellsum (x, visited) -> if visited then cellsum else cellsum + x)))

let solve_p1 inp =
 let parsed_inp = parse_inp inp in
 let rec f boards nums =
  match nums with
  | [] -> failwith "no winner, sth is wrong"
  | hd::tl ->
    (* update state *)
    let boards = List.map ~f:(visit_num hd) boards in
    (* if win then stop *)
    let check_win =
     match List.find ~f:check_lines boards with
     | None -> f boards tl
     | Some winner -> win_output hd winner
    in check_win
 in
 print_endline (string_of_int (Array.length (Array.get (List.hd_exn parsed_inp.boards) 0)));
 print_endline (string_of_int (List.hd_exn (List.tl_exn (parsed_inp.nums))));
 f parsed_inp.boards parsed_inp.nums

let () = print_endline (string_of_int (solve_p1 inp))
