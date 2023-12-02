open Aoc_2023.Day02

let test_parse_game (input : string) (expected : game) =
  let result = parse_game input in
  Alcotest.(check int) "same id" expected.id result.id;
  Alcotest.(check int)
    "same amount of rounds"
    (List.length expected.rounds)
    (List.length result.rounds)
;;

let test_parse_round_line (input : string) (expected : round) =
  let result = parse_round input in
  Alcotest.(check int) "same green" expected.green result.green;
  Alcotest.(check int) "same red" expected.red result.red;
  Alcotest.(check int) "same blue" expected.blue result.blue
;;

let test_solve_part_one () =
  let data =
    [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    ; "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    ; "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    ; "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    ; "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ]
  in
  Alcotest.(check int) "same result" 8 (solve_part_one data)
;;

let test_solve_part_two () =
  let data =
    [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    ; "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    ; "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    ; "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    ; "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ]
  in
  Alcotest.(check int) "same result" 2286 (solve_part_two data)
;;

let () =
  let open Alcotest in
  run
    "Day02"
    [ ( "parse_game_line"
      , [ test_case "parse_game_line" `Quick (fun () ->
            let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" in
            let expected =
              { id = 1
              ; rounds =
                  [ { red = 4; green = 0; blue = 3 }
                  ; { red = 1; green = 2; blue = 6 }
                  ; { red = 0; green = 2; blue = 0 }
                  ]
              }
            in
            test_parse_game input expected)
        ] )
    ; ( "parse_round"
      , [ test_case "all colors" `Quick (fun () ->
            let input = "2 green, 6 blue, 1 red" in
            let expected = { red = 1; green = 2; blue = 6 } in
            test_parse_round_line input expected)
        ; test_case "green missing" `Quick (fun () ->
            let input = "6 blue, 1 red" in
            let expected = { red = 1; green = 0; blue = 6 } in
            test_parse_round_line input expected)
        ] )
    ; ( "solve parts"
      , [ test_case "solve_part_one" `Quick test_solve_part_one
        ; test_case "solve_part_two" `Quick test_solve_part_two
        ] )
    ]
;;
