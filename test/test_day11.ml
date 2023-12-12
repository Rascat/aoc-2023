open Aoc_2023.Day11

let test_data =
  [ "...#......"
  ; ".......#.."
  ; "#........."
  ; ".........."
  ; "......#..."
  ; ".#........"
  ; ".........#"
  ; ".........."
  ; ".......#.."
  ; "#...#....."
  ]
;;

let test_empty_row_indices input expected =
  let actual = empty_row_indices input in
  Alcotest.(check (list int)) "same indices" expected actual
;;

let test_empty_column_indices input expected =
  let actual = empty_column_indices input in
  Alcotest.(check (list int)) "same indices" expected actual
;;

let test_find_all_galaxies input expected =
  let actual = find_all_galaxies input in
  Alcotest.(check (list (pair int int))) "same coordinates" expected actual
;;

let test_compute_distance_after_expansion () =
  let test_case name input expected =
    Alcotest.test_case name `Quick (fun () ->
      let actual = compute_distance_after_expansion input [ 3; 7 ] [ 2; 5; 8 ] in
      Alcotest.(check int) "same distance" expected actual)
  in
  [ test_case "Test case 1" ((1, 5), (4, 10)) 9 ]
;;

let test_solve_part_one () =
  Alcotest.(check int) "same result" 374 (solve_part_one test_data)
;;

let () =
  let open Alcotest in
  run
    "Day11"
    [ ( "test_empty_row_indices"
      , [ test_case "#1" `Quick (fun () ->
            test_empty_row_indices
              (List.map (fun l -> Aoc_2023.Utils.string_to_char_list l) test_data)
              [ 3; 7 ])
        ] )
    ; ( "test_empty_column_indices"
      , [ test_case "#1" `Quick (fun () ->
            test_empty_column_indices
              (List.map (fun l -> Aoc_2023.Utils.string_to_char_list l) test_data)
              [ 2; 5; 8 ])
        ] )
    ; ( "test_find_all_galaxies"
      , [ test_case "#1" `Quick (fun () ->
            test_find_all_galaxies
              (List.map (fun l -> Aoc_2023.Utils.string_to_char_list l) test_data)
              [ 3, 0; 7, 1; 0, 2; 6, 4; 1, 5; 9, 6; 7, 8; 0, 9; 4, 9 ])
        ] )
    ; "test_compute_distance_after_expansion", test_compute_distance_after_expansion ()
    ; "test_solve_part_one", [ test_case "#1" `Quick test_solve_part_one ]
    ]
;;
