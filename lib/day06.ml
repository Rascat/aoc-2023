let find_digits l =
  let open Re.Str in
  let re_numbers = regexp "\\([0-9]+\\)" in
  let rec find_digits' starts_at =
    match search_forward re_numbers l starts_at with
    | exception Not_found -> []
    | start ->
      let s = matched_string l in
      let value = int_of_string s in
      let length = String.length s in
      value :: find_digits' (start + length)
  in
  find_digits' 0
;;

let parse_races data =
  let times = List.hd data |> find_digits in
  let distances = List.nth data 1 |> find_digits in
  List.combine times distances
;;

let compute_distance t_release t_total =
  let t_remaining = t_total - t_release in
  t_remaining * t_release
;;

let compute_nr_winning_strategies time distance =
  let possible_release_times = Utils.range 0 time in
  let actual_distances =
    List.map (fun t -> compute_distance t time) possible_release_times
  in
  List.filter (fun d -> d > distance) actual_distances |> List.length
;;

let solve_part_one _data =
  let races = parse_races _data in
  races
  |> List.map (fun (time, distance) -> compute_nr_winning_strategies time distance)
  |> List.fold_left ( * ) 1
;;

let solve_part_two _data = failwith "Not implemented"
