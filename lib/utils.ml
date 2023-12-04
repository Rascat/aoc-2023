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