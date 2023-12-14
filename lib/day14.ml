let parse_plane (data : string list) =
  List.map (fun l -> Utils.string_to_char_list l) data
  |> Utils.transpose
  |> List.map (fun l -> Utils.char_list_to_string l)
;;

let print_plane plane =
  let rec loop = function
    | [] -> print_endline ""
    | l :: rest ->
      print_endline l;
      loop rest
  in
  loop plane
;;

let find_all_indices c s =
  let chars = Utils.string_to_char_list s in
  let rec aux acc count = function
    | [] -> acc
    | h :: rest ->
      if h = c then aux (count :: acc) (count + 1) rest else aux acc (count + 1) rest
  in
  aux [] 0 chars
;;

let score_column length indices = List.map (fun i -> length - i) indices |> Utils.sum

let solve_part_one data =
  let plane = parse_plane data in
  let sorted_plane =
    List.map
      (fun l ->
        String.split_on_char '#' l
        |> List.map (fun g ->
          Utils.string_to_char_list g
          |> List.sort Char.compare
          |> List.rev
          |> Utils.char_list_to_string)
        |> String.concat "#")
      plane
  in
  print_plane sorted_plane;
  let indices = List.map (fun column -> find_all_indices 'O' column) sorted_plane in
  let scores =
    List.map (fun i -> score_column (String.length (List.hd sorted_plane)) i) indices
  in
  Utils.sum scores
;;
