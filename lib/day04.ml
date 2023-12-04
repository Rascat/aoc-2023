type card =
  { id : int
  ; winning_numbers : int list
  ; own_numbers : int list
  }

let parse_card line =
  let colon_index = String.index line ':' in
  let id = Scanf.sscanf line "Card %d:" (fun id -> id) in
  let numbers_part =
    String.sub line (colon_index + 1) (String.length line - colon_index - 1)
  in
  let winner_own_parts = String.split_on_char '|' numbers_part in
  let winning_numbers =
    String.split_on_char ' ' (List.nth winner_own_parts 0)
    |> List.filter (fun x -> String.length x > 0)
    |> List.map int_of_string
  in
  let own_numbers =
    String.split_on_char ' ' (List.nth winner_own_parts 1)
    |> List.filter (fun x -> String.length x > 0)
    |> List.map int_of_string
  in
  { id; winning_numbers; own_numbers }
;;

let count_matching_numbers winning_numbers own_numbers =
  List.fold_left
    (fun acc number -> if List.mem number winning_numbers then acc + 1 else acc)
    0
    own_numbers
;;

let compute_card_score num_matches =
  Float.to_int
    (if num_matches >= 1 then Float.pow 2.0 (float_of_int (num_matches - 1)) else 0.0)
;;

let solve_part_one _data =
  let cards = _data |> List.map parse_card in
  List.map
    (fun card -> count_matching_numbers card.winning_numbers card.own_numbers)
    cards
  |> List.map compute_card_score
  |> List.fold_left ( + ) 0
;;

let solve_part_two _data = failwith "Not implemented yet"
