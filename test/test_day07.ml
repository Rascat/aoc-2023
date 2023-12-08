open Aoc_2023.Day07

let test_data = [ "32T3K 765"; "T55J5 684"; "KK677 28"; "KTJJT 220"; "QQQJA 483" ]

let test_parse_round input expected =
  let actual = parse_round input in
  Alcotest.(check (list char)) "same cards" expected.cards actual.cards;
  Alcotest.(check int) "same bid" expected.bid actual.bid
;;

let test_hand_from_cards input expected =
  let actual = hand_from_cards input in
  Alcotest.(check int) "same hand" expected (Hand.num actual)
;;

let test_solve_part_one () =
  Alcotest.(check int) "same result" 6440 (solve_part_one test_data)
;;

let test_compare_rounds (r1, r2) expected =
  let actual = compare_rounds r1 r2 in
  Alcotest.(check int) "right order" expected actual
;;

let test_sort_rounds input expected =
  let actual = sort_rounds input in
  List.iter2 (fun a e -> Alcotest.(check int) "same id" e.bid a.bid) actual expected
;;

let () =
  let open Alcotest in
  run
    "Day07"
    [ "solve_part_one", [ test_case "solve part one" `Quick test_solve_part_one ]
    ; ( "test_parse_round"
      , [ test_case "#1" `Quick (fun () ->
            test_parse_round
              "32T3K 765"
              { cards = [ '3'; '2'; 'T'; '3'; 'K' ]; bid = 765 })
        ; test_case "#2" `Quick (fun () ->
            test_parse_round
              "QQQJA 483"
              { cards = [ 'Q'; 'Q'; 'Q'; 'J'; 'A' ]; bid = 483 })
        ] )
    ; ( "test_hand_from_cards"
      , [ test_case "#1" `Quick (fun () ->
            test_hand_from_cards [ '3'; '2'; 'T'; '3'; 'K' ] 2)
        ; test_case "#2" `Quick (fun () ->
            test_hand_from_cards [ '3'; '2'; 'T'; '1'; 'K' ] 1)
        ; test_case "#3" `Quick (fun () ->
            test_hand_from_cards [ '3'; '2'; 'T'; '3'; 'T' ] 3)
        ; test_case "#4" `Quick (fun () ->
            test_hand_from_cards [ '3'; '3'; 'T'; '3'; 'K' ] 4)
        ; test_case "#5" `Quick (fun () ->
            test_hand_from_cards [ '3'; '3'; 'K'; '3'; 'K' ] 5)
        ; test_case "#6" `Quick (fun () ->
            test_hand_from_cards [ '3'; '3'; '3'; '3'; 'K' ] 6)
        ; test_case "#7" `Quick (fun () ->
            test_hand_from_cards [ '3'; '3'; '3'; '3'; '3' ] 7)
        ] )
    ; ( "test_compare_rounds"
      , [ test_case "#1" `Quick (fun () ->
            test_compare_rounds
              ( { cards = [ '3'; '3'; '3'; '3'; '2' ]; bid = 0 }
              , { cards = [ '2'; 'A'; 'A'; 'A'; 'A' ]; bid = 0 } )
              1)
        ; test_case "#2" `Quick (fun () ->
            test_compare_rounds
              ( { cards = [ '7'; '7'; '8'; '8'; '8' ]; bid = 0 }
              , { cards = [ '7'; '7'; '7'; '8'; '8' ]; bid = 0 } )
              1)
        ; test_case "#3" `Quick (fun () ->
            test_compare_rounds
              ( { cards = [ '7'; '7'; '7'; '8'; '8' ]; bid = 0 }
              , { cards = [ '7'; '7'; '8'; '8'; '8' ]; bid = 0 } )
              (-1))
        ; test_case "#4" `Quick (fun () ->
            test_compare_rounds
              ( { cards = [ 'T'; 'J'; '8'; '2'; '3' ]; bid = 0 }
              , { cards = [ '2'; 'T'; 'J'; '7'; 'A' ]; bid = 0 } )
              1)
        ] )
    ; ( "test_sort_rounds"
      , [ test_case "#1" `Quick (fun () ->
            test_sort_rounds
              [ { cards = [ '3'; '2'; 'T'; '3'; 'K' ]; bid = 1 }
              ; { cards = [ 'T'; '5'; '5'; 'J'; '5' ]; bid = 2 }
              ; { cards = [ 'K'; 'K'; '6'; '7'; '7' ]; bid = 3 }
              ; { cards = [ 'K'; 'T'; 'J'; 'J'; 'T' ]; bid = 4 }
              ; { cards = [ 'Q'; 'Q'; 'Q'; 'J'; 'A' ]; bid = 5 }
              ]
              [ { cards = [ '3'; '2'; 'T'; '3'; 'K' ]; bid = 1 }
              ; { cards = [ 'K'; 'T'; 'J'; 'J'; 'T' ]; bid = 4 }
              ; { cards = [ 'K'; 'K'; '6'; '7'; '7' ]; bid = 3 }
              ; { cards = [ 'T'; '5'; '5'; 'J'; '5' ]; bid = 2 }
              ; { cards = [ 'Q'; 'Q'; 'Q'; 'J'; 'A' ]; bid = 5 }
              ])
        ] )
    ]
;;
