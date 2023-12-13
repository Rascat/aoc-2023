let parse_image data = List.map (fun l -> Utils.string_to_char_list l) data

let empty_row_indices image =
  List.mapi
    (fun i row -> if List.for_all (fun c -> c = '.') row then Some i else None)
    image
  |> List.filter (fun o ->
    match o with
    | Some _ -> true
    | None -> false)
  |> List.map (fun o ->
    match o with
    | Some i -> i
    | None -> failwith "Should not contain any None")
;;

let rec transpose matrix =
  match matrix with
  | [] -> []
  | [] :: _ -> []
  | rows ->
    let new_row = List.map List.hd rows in
    let remaining = List.map List.tl rows in
    new_row :: transpose remaining
;;

let empty_column_indices image =
  let transposed_image = transpose image in
  List.mapi
    (fun i row -> if List.for_all (fun c -> c = '.') row then Some i else None)
    transposed_image
  |> List.filter (fun o ->
    match o with
    | Some _ -> true
    | None -> false)
  |> List.map (fun o ->
    match o with
    | Some i -> i
    | None -> failwith "Should not contain any None")
;;

let find_galaxies_in_row row =
  let rec aux row i =
    match row with
    | [] -> []
    | '#' :: rest -> i :: aux rest (i + 1)
    | _ :: rest -> aux rest (i + 1)
  in
  aux row 0
;;

let compute_distance_after_expansion pair empty_rows empty_cols factor =
  match pair with
  | (x1, y1), (x2, y2) ->
    let num_empty_rows_between =
      List.filter (fun i -> (y1 < i && i < y2) || (y2 < i && i < y1)) empty_rows
      |> List.length
    in
    let num_empty_cols_between =
      List.filter (fun i -> (x1 < i && i < x2) || (x2 < i && i < x1)) empty_cols
      |> List.length
    in
    abs (x1 - x2)
    + abs (y1 - y2)
    + (num_empty_cols_between * (factor - 1))
    + (num_empty_rows_between * (factor - 1))
;;

let find_all_galaxies image =
  List.mapi (fun i r -> find_galaxies_in_row r |> List.map (fun x -> x, i)) image
  |> List.flatten
;;

let cartesian l1 l2 =
  List.map (fun e1 -> List.map (fun e2 -> e1, e2) l2) l1 |> List.flatten
;;

let create_pairs l =
  let cp = cartesian l l in
  let rec aux ps acc =
    match ps with
    | [] -> acc
    | ((x1, y1), (x2, y2)) :: rest ->
      if (x1 = x2 && y1 = y2) || List.exists (fun el -> el = ((x2, y2), (x1, y1))) acc
      then aux rest acc
      else aux rest (((x1, y1), (x2, y2)) :: acc)
  in
  aux cp []
;;

let solve_part_one data =
  let image = parse_image data in
  let empty_rows = empty_row_indices image in
  let empty_cols = empty_column_indices image in
  let galaxies = find_all_galaxies image in
  let pairs = create_pairs galaxies in
  List.map (fun p -> compute_distance_after_expansion p empty_rows empty_cols 2) pairs
  |> Utils.sum
;;

let solve_part_two data =
  let image = parse_image data in
  let empty_rows = empty_row_indices image in
  let empty_cols = empty_column_indices image in
  let galaxies = find_all_galaxies image in
  let pairs = create_pairs galaxies in
  List.map
    (fun p -> compute_distance_after_expansion p empty_rows empty_cols 1000000)
    pairs
  |> Utils.sum
;;
