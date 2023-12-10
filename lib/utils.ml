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
