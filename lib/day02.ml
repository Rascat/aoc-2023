type round =
  { red : int
  ; blue : int
  ; green : int
  }

type game =
  { id : int
  ; rounds : round list
  }

let parse_color_red s =
  try Scanf.sscanf s "%s@red: %d" (fun _ red -> red) with
  | Scanf.Scan_failure _ -> 0
;;

let parse_color s re_color =
  let open Re.Str in
  match search_forward re_color s 0 with
  | exception Not_found -> 0
  | _ -> matched_group 1 s |> int_of_string
;;

let parse_round s =
  let open Re.Str in
  let re_red = regexp "\\([0-9]+\\) red" in
  let re_green = regexp "\\([0-9]+\\) green" in
  let re_blue = regexp "\\([0-9]+\\) blue" in
  let red = parse_color s re_red in
  let green = parse_color s re_green in
  let blue = parse_color s re_blue in
  { red; blue; green }
;;

let parse_game line =
  let game_rounds_split = String.split_on_char ':' line in
  let game_part = List.nth game_rounds_split 0 in
  let rounds_part = List.nth game_rounds_split 1 in
  let rounds_data = String.split_on_char ';' rounds_part in
  let id = Scanf.sscanf game_part "Game %d" (fun id -> id) in
  let rounds = List.map parse_round rounds_data in
  { id; rounds }
;;

let power_of_set (set : round) = set.red * set.green * set.blue

let minimal_set_for_rounds rounds =
  let red =
    List.fold_left (fun acc round -> if round.red > acc then round.red else acc) 0 rounds
  in
  let green =
    List.fold_left
      (fun acc round -> if round.green > acc then round.green else acc)
      0
      rounds
  in
  let blue =
    List.fold_left
      (fun acc round -> if round.blue > acc then round.blue else acc)
      0
      rounds
  in
  { red; green; blue }
;;

let solve_part_one data =
  let max_red = 12 in
  let max_green = 13 in
  let max_blue = 14 in
  List.map parse_game data
  |> List.filter (fun game ->
    List.for_all
      (fun round ->
        round.red <= max_red && round.green <= max_green && round.blue <= max_blue)
      game.rounds)
  |> List.fold_left (fun acc game -> acc + game.id) 0
;;

let solve_part_two data =
  List.map parse_game data
  |> List.map (fun game -> minimal_set_for_rounds game.rounds)
  |> List.map power_of_set
  |> List.fold_left ( + ) 0
;;
