open Aoc_2023.Day04

let test_data =
  [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
  ; "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
  ; "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
  ; "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
  ; "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
  ; "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  ]
;;

let test_parse_card input expected =
  let actual = parse_card input in
  Alcotest.(check int) "same id" expected.id actual.id;
  Alcotest.(check (list int))
    "same winning numbers"
    expected.winning_numbers
    actual.winning_numbers;
  Alcotest.(check (list int)) "same own numbers" expected.own_numbers actual.own_numbers
;;

let test_solve_part_one =
  let actual = solve_part_one test_data in
  Alcotest.(check int) "same total" 13 actual
;;

let test_solve_part_two =
  let actual = solve_part_two test_data in
  Alcotest.(check int) "same total" 30 actual
;;

let () =
  let open Alcotest in
  run
    "Day04"
    [ ( "parse_card"
      , [ test_case "parse_card" `Quick (fun () ->
            test_parse_card
              "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
              { id = 1
              ; winning_numbers = [ 41; 48; 83; 86; 17 ]
              ; own_numbers = [ 83; 86; 6; 31; 17; 9; 48; 53 ]
              })
        ] )
    ; ( "solve_part_one"
      , [ test_case "solve_part_one" `Quick (fun () -> test_solve_part_one)
        ; test_case "solve part two" `Quick (fun () -> test_solve_part_two)
        ] )
    ]
;;
