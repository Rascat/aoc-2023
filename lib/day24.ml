type hailstone =
  { position : int * int * int
  ; velocity : int * int * int
  }

type point_2d = int * int

(* computes the intersection of two lines AB and CD *)
let line_intersection_2d (((ax, ay), (bx, by)), ((cx, cy), (dx, dy))) =
  (* Line AB represented as a1x + b1y = c1 *)
  let a1 = by - ay in
  let b1 = ax - bx in
  let c1 = (a1 * ax) + (b1 * ay) in
  (* Line CD represented as a2x + b2y = c2 *)
  let a2 = dy - cy in
  let b2 = cx - dx in
  let c2 = (a2 * cx) + (b2 * cy) in
  let determinant = (a1 * b2) - (a2 * b1) in
  if determinant = 0
  then None
  else (
    let x =
      ((float_of_int b2 *. float_of_int c1) -. (float_of_int b1 *. float_of_int c2))
      /. float_of_int determinant
    in
    let y =
      ((float_of_int a1 *. float_of_int c2) -. (float_of_int a2 *. float_of_int c1))
      /. float_of_int determinant
    in
    Some (x, y))
;;

let parse_hailstone s =
  let open Re.Str in
  let parts = split (regexp " @ ") s in
  let position_coords =
    split (regexp ", ") (List.nth parts 0) |> List.map int_of_string
  in
  let velocity_coors = split (regexp ", ") (List.nth parts 1) |> List.map int_of_string in
  match position_coords, velocity_coors with
  | [ px; py; pz ], [ vx; vy; vz ] -> { position = px, py, pz; velocity = vx, vy, vz }
  | _ -> failwith ("Could not parse hailstone: " ^ s)
;;

let line_2d_from_hailstone { position; velocity } =
  match position, velocity with
  | (px, py, _), (vx, vy, _) -> (px, py), (px + vx, py + vy)
;;

let dot_product (x1, y1) (x2, y2) = (x1 *. x2) +. (y1 *. y2)
let vec_sub (x1, y1) (x2, y2) = x1 -. x2, y1 -. y2
let vec_length (x, y) = sqrt ((x *. x) +. (y *. y))

let create_pairs (l : hailstone list) =
  let cp = Utils.cartesian l l in
  let rec aux (ps : (hailstone * hailstone) list) acc =
    match ps with
    | [] -> acc
    | (h1, h2) :: rest ->
      if h1 = h2 || List.exists (fun el -> el = (h2, h1)) acc
      then aux rest acc
      else aux rest ((h1, h2) :: acc)
  in
  aux cp []
;;

let solve_part_one data =
  let lower = 200000000000000. in
  let upper = 400000000000000. in
  (* let lower = 7. in
     let upper = 27. in *)
  let hailstones = List.map parse_hailstone data in
  let pairs = create_pairs hailstones in
  let x =
    List.filter_map
      (fun (h1, h2) ->
        let intersection_opt =
          line_intersection_2d (line_2d_from_hailstone h1, line_2d_from_hailstone h2)
        in
        match intersection_opt with
        | None -> None
        | Some intersection ->
          (* check that intersection lies before both vectors by computing the dot product *)
          let ph1 =
            match h1 with
            | { position = x, y, _; _ } -> float_of_int x, float_of_int y
          in
          let vh1 =
            match h1 with
            | { velocity = x, y, _; _ } -> float_of_int x, float_of_int y
          in
          let ph2 =
            match h2 with
            | { position = x, y, _; _ } -> float_of_int x, float_of_int y
          in
          let vh2 =
            match h2 with
            | { velocity = x, y, _; _ } -> float_of_int x, float_of_int y
          in
          let v1 = vec_sub intersection ph1 in
          let v2 = vec_sub intersection ph2 in
          let d1 = dot_product v1 vh1 in
          let d2 = dot_product v2 vh2 in
          if d1 < 0. || d2 < 0. then None else Some intersection)
      pairs
    |> List.filter (fun (x, y) -> lower <= x && x <= upper && lower <= y && y <= upper)
  in
  List.length x
;;
