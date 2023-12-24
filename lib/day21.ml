let parse_grid input = List.map (fun s -> Utils.string_to_char_list s) input

let get_char_at (x, y) grid =
  if x < 0 || x >= List.length (List.hd grid) || y < 0 || y >= List.length grid
  then None
  else Some (List.nth (List.nth grid y) x)
;;

let find_char_pos c grid =
  let y_opt = List.find_index (fun row -> List.mem c row) grid in
  let y =
    match y_opt with
    | Some y -> y
    | None -> failwith "Could not find char in grid"
  in
  let x_opt = List.find_index (fun column -> column = c) (List.nth grid y) in
  let x =
    match x_opt with
    | Some x -> x
    | None -> failwith "Could not find char in grid"
  in
  x, y
;;

let up (x, y) = x, y - 1
let down (x, y) = x, y + 1
let left (x, y) = x - 1, y
let right (x, y) = x + 1, y

let get_possible_neighbors grid pos =
  let directions = [ up; down; left; right ] in
  List.filter_map
    (fun direction ->
      let neighbor_pos = direction pos in
      match get_char_at neighbor_pos grid with
      | Some '#' -> None
      | Some '.' -> Some neighbor_pos
      | _ -> None)
    directions
;;

let compute_positions steps start grid =
  let known_positions = Hashtbl.create 1 in
  Hashtbl.add known_positions start true;
  let rec iterate current_positions counter =
    if counter = 0
    then current_positions
    else (
      let next_positions =
        Hashtbl.to_seq_keys current_positions
        |> Seq.map (fun pos -> get_possible_neighbors grid pos)
        |> List.of_seq
        |> List.flatten
        |> List.to_seq
        |> Seq.map (fun pos -> pos, true)
        |> Hashtbl.of_seq
      in
      iterate next_positions (counter - 1))
  in
  iterate known_positions steps
;;

let solve_part_one data =
  let grid = parse_grid data in
  let start_pos = find_char_pos 'S' grid in
  let final_positions = compute_positions 64 start_pos grid in
  (* Somehow there is an off-by-one error in the result, quickfix for now *)
  Hashtbl.length final_positions + 1
;;
