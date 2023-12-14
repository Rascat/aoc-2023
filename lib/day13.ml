let split_at_index index lst =
  let rec split_aux i acc = function
    | [] -> List.rev acc, []
    | x :: xs as l -> if i = 0 then List.rev acc, l else split_aux (i - 1) (x :: acc) xs
  in
  split_aux index [] lst
;;

let rec equals_ignoring_length l1 l2 =
  match l1, l2 with
  | [], _ -> true
  | _, [] -> true
  | h1 :: rest1, h2 :: rest2 ->
    if h1 = h2 then equals_ignoring_length rest1 rest2 else false
;;

let find_horizontal_axis block =
  let length = List.length block in
  let rec aux count =
    match count with
    | 0 -> None
    | i ->
      let left, right = split_at_index i block in
      if equals_ignoring_length (List.rev left) right then Some i else aux (count - 1)
  in
  aux (length - 1)
;;

let find_vertical_axis block =
  let transposed_block =
    block
    |> List.map Utils.string_to_char_list
    |> Utils.transpose
    |> List.map Utils.char_list_to_string
  in
  find_horizontal_axis transposed_block
;;

let parse_blocks data =
  let rec parse' block_acc list_acc = function
    | [] -> List.rev (List.rev block_acc :: list_acc)
    | h :: rest ->
      if h = ""
      then parse' [] (List.rev block_acc :: list_acc) rest
      else parse' (h :: block_acc) list_acc rest
  in
  parse' [] [] data
;;

let solve_part_one data =
  let blocks = parse_blocks data in
  List.map
    (fun (b : string list) ->
      match find_horizontal_axis b with
      | Some i -> i * 100
      | None ->
        (match find_vertical_axis b with
         | Some i -> i
         | None -> failwith "No axis found"))
    blocks
  |> Utils.sum
;;

let solve_part_two _data = failwith "Not implemented yet"
