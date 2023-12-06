open Aoc_2023.Day06

let _test_data = [ "Time:      7  15   30"; "Distance:  9  40  200" ]

let test_parse_races input expected =
  Alcotest.(check (list (pair int int)))
    "same time/distance pairs"
    expected
    (parse_races input)
;;

let test_compute_distance (t_release, t_total) expected =
  Alcotest.(check int) "same distance" expected (compute_distance t_release t_total)
;;

let () =
  let open Alcotest in
  run
    "Day06"
    [ ( "parse_races"
      , [ test_case "parse_races #1" `Quick (fun () ->
            test_parse_races _test_data [ 7, 9; 15, 40; 30, 200 ])
        ] )
    ; ( "test_compute_distance"
      , [ test_case "#1" `Quick (fun () -> test_compute_distance (0, 7) 0)
        ; test_case "#2" `Quick (fun () -> test_compute_distance (1, 7) 6)
        ; test_case "#3" `Quick (fun () -> test_compute_distance (2, 7) 10)
        ; test_case "#4" `Quick (fun () -> test_compute_distance (3, 7) 12)
        ; test_case "#5" `Quick (fun () -> test_compute_distance (4, 7) 12)
        ; test_case "#6" `Quick (fun () -> test_compute_distance (5, 7) 10)
        ; test_case "#7" `Quick (fun () -> test_compute_distance (6, 7) 6)
        ; test_case "#8" `Quick (fun () -> test_compute_distance (7, 7) 0)
        ] )
    ]
;;
