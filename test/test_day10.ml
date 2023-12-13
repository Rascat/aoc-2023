open Aoc_2023.Day10

let test_data = [ "....."; ".S-7."; ".|.|."; ".L-J."; "....." ]

let test_solve_part_one () =
  Alcotest.(check int) "same result" 4 (solve_part_one test_data)
;;

let () =
  let open Alcotest in
  run "Day10" [ "test_solve_part_one", [ test_case "#1" `Quick test_solve_part_one ] ]
;;
