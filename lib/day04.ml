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

let rec inc_next_n_by curr_index n inc deck =
  if n = 0
  then deck
  else (
    let next_index = (curr_index + 1) mod Array.length deck in
    deck.(next_index) <- deck.(next_index) + inc;
    inc_next_n_by next_index (n - 1) inc deck)
;;

let solve_part_one _data =
  let cards = _data |> List.map parse_card in
  List.map
    (fun card -> count_matching_numbers card.winning_numbers card.own_numbers)
    cards
  |> List.map compute_card_score
  |> Utils.sum
;;

let solve_part_two _data =
  let cards = _data |> List.map parse_card in
  let deck = Array.make (List.length cards) 1 in
  let rec loop cards deck =
    match cards with
    | [] -> deck
    | card :: rest ->
      let num_matches = count_matching_numbers card.winning_numbers card.own_numbers in
      let curr_increment = deck.(card.id - 1) in
      (* pretty print deck *)
      Array.iter (fun x -> Printf.printf "%d " x) deck;
      Printf.printf "\n";
      loop rest (inc_next_n_by (card.id - 1) num_matches curr_increment deck)
  in
  loop cards deck |> Array.fold_left ( + ) 0
;;
