open Aoc_2023.Day08

let test_data1 =
  [ "RL"
  ; ""
  ; "AAA = (BBB, CCC)"
  ; "BBB = (DDD, EEE)"
  ; "CCC = (ZZZ, GGG)"
  ; "DDD = (DDD, DDD)"
  ; "EEE = (EEE, EEE)"
  ; "GGG = (GGG, GGG)"
  ; "ZZZ = (ZZZ, ZZZ)"
  ]
;;

let test_data2 =
  [ "LR"
  ; ""
  ; "11A = (11B, XXX)"
  ; "11B = (XXX, 11Z)"
  ; "11Z = (11B, XXX)"
  ; "22A = (22B, XXX)"
  ; "22B = (22C, 22C)"
  ; "22C = (22Z, 22Z)"
  ; "22Z = (22B, 22B)"
  ; "XXX = (XXX, XXX)"
  ]
;;

let test_solve_part_one () =
  Alcotest.(check int) "same result" 2 (solve_part_one test_data1)
;;

let test_solve_part_two () =
  Alcotest.(check int) "same result" 6 (solve_part_two test_data2)
;;

let () =
  let open Alcotest in
  run
    "Day08"
    [ "solve_part_one", [ test_case "solve part one" `Quick test_solve_part_one ]
    ; "solve_part_two", [ test_case "solve part two" `Quick test_solve_part_two ]
    ]
;;
