let parse_grid data = List.map (fun l -> Utils.string_to_char_list l) data

let get_char_at (x, y) grid =
  if x < 0 || x >= List.length (List.hd grid) || y < 0 || y >= List.length grid
  then None
  else Some (List.nth (List.nth grid y) x)
;;

let print_coord (x, y) = print_endline (Printf.sprintf "(%d, %d)" x y)

let rec print_visited = function
  | [] -> print_endline ""
  | (x, y) :: rest ->
    print_coord (x, y);
    print_visited rest
;;

let move_up (x, y) = x, y - 1
let move_down (x, y) = x, y + 1
let move_left (x, y) = x - 1, y
let move_right (x, y) = x + 1, y
let move_straight (x1, y1) (x2, y2) = x2 + (x2 - x1), y2 + (y2 - y1)

(* / *)
let right_deflect (x1, y1) (x2, y2) =
  match compare x1 x2 with
  | -1 -> move_up (x2, y2)
  | 1 -> move_down (x2, y2)
  | 0 ->
    (match compare y1 y2 with
     | -1 -> move_left (x2, y2)
     | 1 -> move_right (x2, y2)
     | _ -> failwith "Error in right_deflect: p1 should not equal p2")
  | _ -> failwith "Error in right_deflect: compare returned unexpected result"
;;

(* \ *)
let left_deflect (x1, y1) (x2, y2) =
  match compare x1 x2 with
  | -1 -> move_down (x2, y2)
  | 1 -> move_up (x2, y2)
  | 0 ->
    (match compare y1 y2 with
     (* j *)
     | -1 -> move_right (x2, y2)
     | 1 -> move_left (x2, y2)
     | _ -> failwith "Error in right_deflect: p1 should not equal p2")
  | _ -> failwith "Error in right_deflect: compare x1 x2 returned unexpected result"
;;

let move_up (x, y) = x, y - 1
let move_down (x, y) = x, y + 1
let move_left (x, y) = x - 1, y
let move_right (x, y) = x + 1, y

(*
   A function which given a current position p1 and a last position p0
   computes the next position(s) and returns the list of all visited positions
*)
let simulate_beam grid =
  let junction_map = Hashtbl.create 100 in
  let rec walk visited last_pos current_pos =
    match current_pos with
    | x, y ->
      print_coord (x, y);
      (match get_char_at (x, y) grid with
       | None -> visited
       | Some c ->
         (match c with
          | '.' -> walk ((x, y) :: visited) (x, y) (move_straight last_pos (x, y))
          | '/' -> walk ((x, y) :: visited) (x, y) (right_deflect last_pos (x, y))
          | '\\' -> walk ((x, y) :: visited) (x, y) (left_deflect last_pos (x, y))
          | '|' ->
            (match last_pos with
             | latest_x, _ ->
               if x - latest_x = 0
               then walk ((x, y) :: visited) (x, y) (move_straight last_pos (x, y))
               else (
                 match Hashtbl.find_opt junction_map (x, y) with
                 | None ->
                   Hashtbl.add junction_map (x, y) true;
                   walk ((x, y) :: visited) (x, y) (move_up (x, y))
                   @ walk ((x, y) :: visited) (x, y) (move_down (x, y))
                 | Some _ -> visited))
          | '-' ->
            (match last_pos with
             | _, latest_y ->
               if y - latest_y = 0
               then walk ((x, y) :: visited) (x, y) (move_straight last_pos (x, y))
               else (
                 match Hashtbl.find_opt junction_map (x, y) with
                 | None ->
                   Hashtbl.add junction_map (x, y) true;
                   walk ((x, y) :: visited) (x, y) (move_left (x, y))
                   @ walk ((x, y) :: visited) (x, y) (move_right (x, y))
                 | Some _ -> visited))
          | _ as t -> failwith ("encountered unexpected tile: " ^ String.make 1 t)))
  in
  walk [] (-1, 0) (0, 0)
;;

let solve_part_one data =
  let grid = parse_grid data in
  (* Utils.print_char_grid grid; *)
  let visited = simulate_beam grid in
  print_visited visited;
  List.sort_uniq (fun p1 p2 -> compare p1 p2) visited |> List.length
;;

let solve_part_two _data = failwith "Not implemented yet"
