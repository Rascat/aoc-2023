type direction =
  | Up
  | Down
  | Left
  | Right

type ray =
  { position : int * int
  ; direction : direction
  }

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

let move_straight { position; direction } =
  match direction with
  | Up -> { position = move_up position; direction }
  | Down -> { position = move_down position; direction }
  | Left -> { position = move_left position; direction }
  | Right -> { position = move_right position; direction }
;;

(* / *)
let right_deflect { position; direction } =
  match direction with
  | Up -> { position = move_right position; direction = Right }
  | Down -> { position = move_left position; direction = Left }
  | Left -> { position = move_down position; direction = Down }
  | Right -> { position = move_up position; direction = Up }
;;

(* \ *)
let left_deflect { position; direction } =
  match direction with
  | Up -> { position = move_left position; direction = Left }
  | Down -> { position = move_right position; direction = Right }
  | Left -> { position = move_up position; direction = Up }
  | Right -> { position = move_down position; direction = Down }
;;

(*
   A function which given a current position p1 and a last position p0
   computes the next position(s) and returns the list of all visited positions
*)
let simulate_beam grid =
  let junction_map = Hashtbl.create 100 in
  let rec walk visited ({ position = x, y; direction } as ray) =
    match get_char_at (x, y) grid with
    | None -> visited
    | Some c ->
      (match c with
       | '.' -> walk ((x, y) :: visited) (move_straight ray)
       | '/' -> walk ((x, y) :: visited) (right_deflect ray)
       | '\\' -> walk ((x, y) :: visited) (left_deflect ray)
       | '|' ->
         (match direction with
          | Up | Down -> walk ((x, y) :: visited) (move_straight ray)
          | Left | Right ->
            (match Hashtbl.find_opt junction_map (x, y) with
             | None ->
               Hashtbl.add junction_map (x, y) true;
               walk ((x, y) :: visited) { position = move_up (x, y); direction = Up }
               @ walk
                   ((x, y) :: visited)
                   { position = move_down (x, y); direction = Down }
             | Some _ -> visited))
       | '-' ->
         (match direction with
          | Left | Right -> walk ((x, y) :: visited) (move_straight ray)
          | Up | Down ->
            (match Hashtbl.find_opt junction_map (x, y) with
             | None ->
               Hashtbl.add junction_map (x, y) true;
               walk ((x, y) :: visited) { position = move_left (x, y); direction = Left }
               @ walk
                   ((x, y) :: visited)
                   { position = move_right (x, y); direction = Right }
             | Some _ -> visited))
       | _ as t -> failwith ("encountered unexpected tile: " ^ String.make 1 t))
  in
  walk [] { position = 0, 0; direction = Right }
;;

let solve_part_one data =
  let grid = parse_grid data in
  let visited = simulate_beam grid in
  List.sort_uniq (fun p1 p2 -> compare p1 p2) visited |> List.length
;;

let solve_part_two _data = failwith "Not implemented yet"
