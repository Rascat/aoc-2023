open Aoc_2023.Day16

let test_data =
  [ ".|...\\...."
  ; "|.-.\\....."
  ; ".....|-..."
  ; "........|."
  ; ".........."
  ; ".........\\"
  ; "..../.\\\\.."
  ; ".-.-/..|.."
  ; ".|....-|.\\"
  ; "..//.|...."
  ]
;;

let test_solve_part_one () =
  Alcotest.(check int) "same result" 46 (solve_part_one test_data)
;;

let () =
  let open Alcotest in
  run "Day16" [ "test_solve_part_one", [ test_case "#1" `Quick test_solve_part_one ] ]
;;
