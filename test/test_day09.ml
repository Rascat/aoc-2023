open Aoc_2023.Day09

let test_data = [ "0 3 6 9 12 15"; "1 3 6 10 15 21"; "10 13 16 21 30 45" ]

let test_differences input expected =
  Alcotest.(check (list int)) "same list" expected (differences input)
;;

let test_generate_sequences input expected =
  Alcotest.(check (list (list int))) "same sequences" expected (generate_sequences input)
;;

let test_extrapolate_sequences input expected =
  Alcotest.(check (list int)) "same sequence" expected (extrapolate_sequences input)
;;

let test_solve_part_one () =
  Alcotest.(check int) "same result" 114 (solve_part_one test_data)
;;

let () =
  let open Alcotest in
  run
    "Day09"
    [ ( "test_differences"
      , [ test_case "empty input" `Quick (fun () -> test_differences [] [])
        ; test_case "standard #1" `Quick (fun () ->
            test_differences [ 0; 3; 6; 9; 12; 15 ] [ 3; 3; 3; 3; 3 ])
        ; test_case "standard #2" `Quick (fun () ->
            test_differences [ 3; 3; 3; 3; 3 ] [ 0; 0; 0; 0 ])
        ] )
    ; ( "test_generate_sequences"
      , [ test_case "#1" `Quick (fun () ->
            test_generate_sequences
              [ 0; 3; 6; 9; 12; 15 ]
              [ [ 0; 3; 6; 9; 12; 15 ]; [ 3; 3; 3; 3; 3 ]; [ 0; 0; 0; 0 ] ])
        ; test_case "#2" `Quick (fun () ->
            test_generate_sequences
              [ 1; 3; 6; 10; 15; 21 ]
              [ [ 1; 3; 6; 10; 15; 21 ]; [ 2; 3; 4; 5; 6 ]; [ 1; 1; 1; 1 ]; [ 0; 0; 0 ] ])
        ] )
    ; ( "test_extrapolate_sequences"
      , [ test_case "#1" `Quick (fun () ->
            test_extrapolate_sequences
              [ [ 0; 3; 6; 9; 12; 15 ]; [ 3; 3; 3; 3; 3 ]; [ 0; 0; 0; 0 ] ]
              [ 0; 3; 18 ])
        ; test_case "#2" `Quick (fun () ->
            test_extrapolate_sequences
              [ [ 1; 3; 6; 10; 15; 21 ]; [ 2; 3; 4; 5; 6 ]; [ 1; 1; 1; 1 ]; [ 0; 0; 0 ] ]
              [ 0; 1; 7; 28 ])
        ] )
    ; "test_solve_part_one", [ test_case "solve part one" `Quick test_solve_part_one ]
    ]
;;
