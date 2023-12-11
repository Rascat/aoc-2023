let parse_maze input = List.map (fun l -> Utils.string_to_char_list l) input

let pipe_at (x, y) maze =
  let row = List.nth maze y in
  List.nth row x
;;

let find_start_pos maze =
  let y =
    match List.find_index (fun y -> List.exists (fun x -> x = 'S') y) maze with
    | Some y -> y
    | None -> failwith "No row with start symbol found"
  in
  let x =
    match List.find_index (fun x -> x = 'S') (List.nth maze y) with
    | Some x -> x
    | None -> failwith "No column with start symbol found in row"
  in
  x, y
;;

let walk_maze maze =
  let start = find_start_pos maze in
  let rec walk' prev_pos curr_pos maze acc =
    let pipe = pipe_at curr_pos maze in
    let prev_x, prev_y = prev_pos in
    let curr_x, curr_y = curr_pos in
    print_endline (Printf.sprintf "pos: (%d, %d)" curr_x curr_y);
    if pipe = 'S' && acc > 0
    then acc
    else (
      match pipe with
      | '|' ->
        let next_y = if prev_y < curr_y then curr_y + 1 else curr_y - 1 in
        walk' curr_pos (curr_x, next_y) maze (acc + 1)
      | '-' ->
        let next_x = if prev_x < curr_x then curr_x + 1 else curr_x - 1 in
        walk' curr_pos (next_x, curr_y) maze (acc + 1)
      | 'L' ->
        let next_pos =
          if prev_y < curr_y then curr_x + 1, curr_y else curr_x, curr_y - 1
        in
        walk' curr_pos next_pos maze (acc + 1)
      | 'J' ->
        let next_pos =
          if prev_y < curr_y then curr_x - 1, curr_y else curr_x, curr_y - 1
        in
        walk' curr_pos next_pos maze (acc + 1)
      | '7' ->
        let next_pos =
          if prev_y > curr_y then curr_x - 1, curr_y else curr_x, curr_y + 1
        in
        walk' curr_pos next_pos maze (acc + 1)
      | 'F' ->
        let next_pos =
          if prev_y > curr_y then curr_x + 1, curr_y else curr_x, curr_y + 1
        in
        walk' curr_pos next_pos maze (acc + 1)
      | '.' -> failwith "Something went wrong -> landed on ground"
      | _ -> failwith "Invalid state")
  in
  match start with
  | (start_x, start_y) -> walk' (start_x, start_y) (start_x, start_y + 1) maze 1
  
;;

let solve_part_one data = 
  let maze = parse_maze data in
  (walk_maze maze) / 2
let solve_part_two _data = failwith "Not implemented yet"
