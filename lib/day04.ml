open Base

type card =
  { id : int
  ; winning_numbers : int list
  ; own_numbers : int list
  }

let parse_card line =
  let colon_index =
    match String.index line ':' with
    | Some i -> i
    | None -> failwith "No colon found"
  in
  let id = Stdlib.Scanf.sscanf line "Card %d:" (fun id -> id) in
  let numbers_part =
    String.sub line ~pos:(colon_index + 1) ~len:(String.length line - colon_index - 1)
  in
  let winner_own_parts = String.split ~on:'|' numbers_part in
  let winning_numbers =
    String.split
      (match List.nth winner_own_parts 0 with
       | Some x -> x
       | None -> failwith "No winning numbers")
      ~on:' '
    |> List.filter ~f:(fun x -> String.length x > 0)
    |> List.map ~f:Int.of_string
  in
  let own_numbers =
    String.split
      ~on:' '
      (match List.nth winner_own_parts 1 with
       | Some x -> x
       | None -> failwith "No own numbers")
    |> List.filter ~f:(fun x -> String.length x > 0)
    |> List.map ~f:Int.of_string
  in
  { id; winning_numbers; own_numbers }
;;

let count_matching_numbers winning_numbers own_numbers =
  List.fold
    own_numbers
    ~f:(fun acc number ->
      if List.mem winning_numbers number ~equal:(fun x y -> x = y) then acc + 1 else acc)
    ~init:0
;;

let compute_card_score num_matches =
  if num_matches >= 1 then 2 ** (num_matches - 1) else 0
;;

let solve_part_one data =
  List.map ~f:parse_card data
  |> List.map ~f:(fun card ->
    count_matching_numbers card.winning_numbers card.own_numbers)
  |> List.map ~f:compute_card_score
  |> Utils.sum
;;

let rec inc_next_n_by curr_index n inc deck =
  if n = 0
  then deck
  else (
    let next_index = (curr_index + 1) % Array.length deck in
    deck.(next_index) <- deck.(next_index) + inc;
    inc_next_n_by next_index (n - 1) inc deck)
;;

let solve_part_two data =
  let cards = data |> List.map ~f:parse_card in
  let deck = Array.init (List.length cards) ~f:(fun _ -> 1) in
  let rec loop cards deck =
    match cards with
    | [] -> deck
    | card :: rest ->
      let num_matches = count_matching_numbers card.winning_numbers card.own_numbers in
      let curr_increment = deck.(card.id - 1) in
      loop rest (inc_next_n_by (card.id - 1) num_matches curr_increment deck)
  in
  loop cards deck |> Array.fold ~f:( + ) ~init:0
;;
