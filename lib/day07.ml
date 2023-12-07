open Base
open Stdio

type round =
  { cards : char list
  ; bid : int
  }

let print_round round =
  List.iter round.cards ~f:(fun c -> printf "%c " c);
  print_endline ""
;;

module Hand = struct
  type t =
    | FiveOfKind
    | FourOfKind
    | FullHouse
    | ThreeOfKind
    | TwoPair
    | OnePair
    | HighCard

  let num = function
    | FiveOfKind -> 7
    | FourOfKind -> 6
    | FullHouse -> 5
    | ThreeOfKind -> 4
    | TwoPair -> 3
    | OnePair -> 2
    | HighCard -> 1
  ;;
end

let num_of_card = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | d -> Char.get_digit_exn d
;;

let hand_from_cards (cards : char list) =
  let grouped = List.sort cards ~compare:Char.compare |> List.group ~break:Char.( <> ) in
  match grouped with
  | [ [ _ ]; [ _ ]; [ _ ]; [ _ ]; [ _ ] ] -> Hand.HighCard
  | [ [ _ ]; [ _ ]; [ _ ]; [ _; _ ] ]
  | [ [ _ ]; [ _ ]; [ _; _ ]; [ _ ] ]
  | [ [ _ ]; [ _; _ ]; [ _ ]; [ _ ] ]
  | [ [ _; _ ]; [ _ ]; [ _ ]; [ _ ] ] -> Hand.OnePair
  | [ [ _; _ ]; [ _; _ ]; _ ] | [ [ _; _ ]; _; [ _; _ ] ] | [ _; [ _; _ ]; [ _; _ ] ] ->
    Hand.TwoPair
  | [ [ _; _; _ ]; [ _ ]; [ _ ] ]
  | [ [ _ ]; [ _; _; _ ]; [ _ ] ]
  | [ [ _ ]; [ _ ]; [ _; _; _ ] ] -> Hand.ThreeOfKind
  | [ [ _; _; _ ]; [ _; _ ] ] | [ [ _; _ ]; [ _; _; _ ] ] -> Hand.FullHouse
  | [ [ _; _; _; _ ]; [ _ ] ] | [ [ _ ]; [ _; _; _; _ ] ] -> Hand.FourOfKind
  | [ [ _; _; _; _; _ ] ] -> Hand.FiveOfKind
  | _ -> failwith "Pattern should not appear"
;;

let compare_card_by_card (cs1 : char list) (cs2 : char list) =
  let pairs = List.zip_exn cs1 cs2 in
  let rec loop = function
    | [] -> 0
    | (c1, c2) :: rest ->
      printf "comp: %c - %c\n" c1 c2;
      if Char.( <> ) c1 c2
      then Int.compare (num_of_card c1) (num_of_card c2)
      else loop rest
  in
  loop pairs
;;

let parse_round l =
  let parts = String.split ~on:' ' l in
  let cards = List.nth_exn parts 0 |> Utils.string_to_char_list in
  let bid = List.nth_exn parts 1 |> Int.of_string in
  { cards; bid }
;;

let compare_rounds r1 r2 =
  let h1 = hand_from_cards r1.cards |> Hand.num in
  let h2 = hand_from_cards r2.cards |> Hand.num in
  if h1 <> h2 then Int.compare h1 h2 else compare_card_by_card r1.cards r2.cards
;;

let sort_rounds rounds = List.sort rounds ~compare:compare_rounds

let solve_part_one data =
  let rounds = List.map ~f:parse_round data in
  let sorted_rounds = rounds
  |> sort_rounds in
  List.iter sorted_rounds ~f:(fun r -> print_round r);
  List.foldi sorted_rounds ~init:0 ~f:(fun i acc r ->
    acc + ((i + 1) * r.bid))
;;

let solve_part_two _data = failwith "Not implemented"
