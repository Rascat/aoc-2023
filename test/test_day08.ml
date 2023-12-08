open Aoc_2023.Day08

let test_data =
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

let test_solve_part_one () =
  Alcotest.(check int) "same result" 2 (solve_part_one test_data)
;;

let () =
  let open Alcotest in
  run
    "Day08"
    [ "solve_part_one", [ test_case "solve part one" `Quick test_solve_part_one ] ]
;;
