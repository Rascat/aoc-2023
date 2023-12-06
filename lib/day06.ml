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

let find_digits_as_strings l =
  let open Re.Str in
  let re_numbers = regexp "\\([0-9]+\\)" in
  let rec find_digits' starts_at =
    match search_forward re_numbers l starts_at with
    | exception Not_found -> []
    | start ->
      let s = matched_string l in
      let length = String.length s in
      s :: find_digits' (start + length)
  in
  find_digits' 0
;;

let parse_races data =
  let times = List.hd data |> find_digits in
  let distances = List.nth data 1 |> find_digits in
  List.combine times distances
;;

let parse_races2 data =
  let time_parts = List.nth data 0 |> find_digits_as_strings in
  let distance_parts = List.nth data 1 |> find_digits_as_strings in
  let time = List.fold_left (fun i p -> i ^ p) "" time_parts |> int_of_string in
  let distance = List.fold_left (fun i p -> i ^ p) "" distance_parts |> int_of_string in
  [ time, distance ]
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

let compute_nr_winning_strategies_seq time distance =
  Utils.range_seq 0 time
  |> Seq.map (fun t -> compute_distance t time)
  |> Seq.filter (fun d -> d > distance)
  |> Seq.length
;;

let solve_part_one _data =
  let races = parse_races _data in
  races
  |> List.map (fun (time, distance) -> compute_nr_winning_strategies time distance)
  |> List.fold_left ( * ) 1
;;

let solve_part_two _data =
  let races = parse_races2 _data in
  races
  |> List.map (fun (time, distance) -> compute_nr_winning_strategies_seq time distance)
  |> List.fold_left ( * ) 1
;;
