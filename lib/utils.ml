let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with
    | End_of_file -> None
  in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
      close_in ic;
      List.rev acc
  in
  loop []
;;

let read_blocks name =
  let open Stdio in
  let open Re.Str in
  In_channel.read_all name
  |> split (regexp "\n\n")
  |> List.map (fun l -> split (regexp "\n") l)
;;

let string_to_char_list s = s |> String.to_seq |> List.of_seq
let char_list_to_string l = l |> List.to_seq |> String.of_seq
let string_of_char c = String.make 1 c
let sum l = List.fold_left ( + ) 0 l

let range start length =
  let rec range' start length acc =
    if length = 0 then acc else range' (start + 1) (length - 1) (start :: acc)
  in
  range' start length []
;;

let range_seq start length =
  let rec aux current end_ () =
    if current > end_ then Seq.Nil else Seq.Cons (current, aux (current + 1) end_)
  in
  aux start (start + length - 1)
;;

let find_digits l =
  let open Re.Str in
  let re_numbers = regexp "\\(-?[0-9]+\\)" in
  let rec find_digits' starts_at =
    match search_forward re_numbers l starts_at with
    | exception Not_found -> []
    | start ->
      let s = matched_string l in
      let value = int_of_string s in
      let length = String.length s in
      value :: find_digits' (start + length)
  in
  find_digits' 0
;;

let rec transpose matrix =
  match matrix with
  | [] -> []
  | [] :: _ -> []
  | rows ->
    let new_row = List.map List.hd rows in
    let remaining = List.map List.tl rows in
    new_row :: transpose remaining
;;


let print_str_list list =
  let rec loop = function
    | [] -> print_endline ""
    | l :: rest ->
      print_endline l;
      loop rest
  in
  loop list
;;