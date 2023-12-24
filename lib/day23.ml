module Grid = struct
  type 'a t = 'a list list
  type position = int * int

  let up (x, y) = x, y - 1
  let down (x, y) = x, y + 1
  let left (x, y) = x - 1, y
  let right (x, y) = x + 1, y

  let neighbours current_pos =
    let directions = [ up; down; left; right ] in
    List.map (fun direction -> direction current_pos) directions
  ;;

  let get_element_at (x, y) ~grid =
    if x < 0 || x >= List.length (List.hd grid) || y < 0 || y >= List.length grid
    then None
    else Some (List.nth (List.nth grid y) x)
  ;;

  let position_to_string (x, y) = Printf.sprintf "(%d, %d)" x y
end

let next_steps current_pos visited grid =
  let neighbours = Grid.neighbours current_pos in
  List.filter_map
    (fun n ->
      if List.mem n visited
      then None
      else (
        match Grid.get_element_at n ~grid with
        | Some '#' -> None
        | Some '.' -> Some n
        | Some '<' | Some '>' | Some '^' | Some 'v' -> Some n
        | _ -> None))
    neighbours
;;

let next_steps2 current_pos visited grid =
  let neighbours = Grid.neighbours current_pos in
  List.filter_map
    (fun n ->
      if Hashtbl.mem visited n
      then None
      else (
        match Grid.get_element_at n ~grid with
        | Some '#' -> None
        | Some '.' -> Some n
        | Some '<' | Some '>' | Some '^' | Some 'v' -> Some n
        | _ -> None))
    neighbours
;;

let walk grid =
  let start_pos =
    List.find_index (fun col -> col = '.') (List.hd grid) |> Option.get, 0
  in
  let dest_pos =
    ( List.find_index (fun col -> col = '.') (List.rev grid |> List.hd) |> Option.get
    , List.length grid - 1 )
  in
  print_endline
    ("start: "
     ^ Grid.position_to_string start_pos
     ^ "; destination: "
     ^ Grid.position_to_string dest_pos);
  let rec walk' current_pos grid visited (acc : (int * int) list list) =
    (* print_endline (Grid.position_to_string current_pos); *)
    if List.mem current_pos visited
    then
      (* print_endline ("Already seen position: " ^ Grid.position_to_string current_pos); *)
      acc
    else if current_pos = dest_pos
    then (
      print_endline "Destination reached!";
      visited :: acc)
    else (
      let updated_visited = current_pos :: visited in
      match Grid.get_element_at current_pos ~grid with
      | Some '>' -> walk' (Grid.right current_pos) grid updated_visited acc
      | Some '<' -> walk' (Grid.left current_pos) grid updated_visited acc
      | Some '^' -> walk' (Grid.up current_pos) grid updated_visited acc
      | Some 'v' -> walk' (Grid.down current_pos) grid updated_visited acc
      | Some '.' ->
        let steps = next_steps current_pos visited grid in
        (match steps with
         | [] -> acc
         | [ a ] -> walk' a grid updated_visited acc @ acc
         | [ a; b ] ->
           walk' a grid updated_visited acc @ walk' b grid updated_visited acc @ acc
         | [ a; b; c ] ->
           walk' a grid updated_visited acc
           @ walk' b grid updated_visited acc
           @ walk' c grid updated_visited acc
           @ acc
         | _ -> failwith "oops")
      | _ -> failwith "Should not have happened")
  in
  walk' start_pos grid [] []
;;

let walk2 grid =
  let start_pos =
    List.find_index (fun col -> col = '.') (List.hd grid) |> Option.get, 0
  in
  let dest_pos =
    ( List.find_index (fun col -> col = '.') (List.rev grid |> List.hd) |> Option.get
    , List.length grid - 1 )
  in
  print_endline
    ("start: "
     ^ Grid.position_to_string start_pos
     ^ "; destination: "
     ^ Grid.position_to_string dest_pos);
  let rec walk' current_pos grid visited (acc : (int * int) list list) =
    (* print_endline (Grid.position_to_string current_pos); *)
    if Hashtbl.mem visited current_pos
    then
      (* print_endline ("Already seen position: " ^ Grid.position_to_string current_pos); *)
      acc
    else if current_pos = dest_pos
    then (
      print_endline "Destination reached!";
      (Hashtbl.to_seq_keys visited |> List.of_seq) :: acc)
    else (
      let updated_visited = Hashtbl.copy visited in
      Hashtbl.add updated_visited current_pos true;
      match Grid.get_element_at current_pos ~grid with
      | Some '>' | Some '<' | Some '^' | Some 'v' | Some '.' ->
        let steps = next_steps2 current_pos (Hashtbl.copy visited) grid in
        (match steps with
         | [] -> acc
         | [ a ] -> walk' a grid updated_visited acc @ acc
         | [ a; b ] ->
           walk' a grid updated_visited acc @ walk' b grid updated_visited acc @ acc
         | [ a; b; c ] ->
           walk' a grid updated_visited acc
           @ walk' b grid updated_visited acc
           @ walk' c grid updated_visited acc
           @ acc
         | _ -> failwith "oops")
      | _ -> failwith "Should not have happened")
  in
  walk' start_pos grid (Hashtbl.create 1000) []
;;

let parse_grid (lines : string list) = List.map Utils.string_to_char_list lines

let solve_part_one data =
  let grid = parse_grid data in
  let result = walk grid in
  List.map (fun p -> List.length p) result |> List.fold_left max min_int
;;

let solve_part_two data =
  let grid = parse_grid data in
  let result = walk2 grid in
  List.map (fun p -> List.length p) result |> List.fold_left max min_int
;;
