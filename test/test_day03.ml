open Aoc_2023.Day03

let test_parse_digit_meta (input : string) expected =
  let actual = parse_digit_meta input 0 in
  Alcotest.(check int)
    "lists have same length"
    (List.length expected)
    (List.length actual);
  List.iter2
    (fun expected_meta actual_meta ->
      Alcotest.(check int) "same value" expected_meta.value actual_meta.value;
      Alcotest.(check int) "same line_nr" expected_meta.line_nr actual_meta.line_nr;
      Alcotest.(check int) "same starts_at" expected_meta.starts_at actual_meta.starts_at;
      Alcotest.(check int) "same length" expected_meta.length actual_meta.length)
    expected
    actual
;;

let test_check_adjacent_positions input_meta input_data expected =
  let actual = check_adjacent_positions input_meta input_data in
  Alcotest.(check bool) "expected boolean" expected actual
;;

let test_solve_part_one () =
  let input =
    [ "467..114.."
    ; "...*......"
    ; "..35..633."
    ; "......#..."
    ; "617*......"
    ; ".....+.58."
    ; "..592....."
    ; "......755."
    ; "...$.*...."
    ; ".664.598.."
    ]
  in
  Alcotest.(check int) "solve_part_one" 4361 (Aoc_2023.Day03.solve_part_one input)
;;

let () =
  let open Alcotest in
  run
    "Day03"
    [ "solve parts", [ test_case "solve_part_one" `Quick test_solve_part_one ]
    ; ( "parse_digit_meta"
      , [ test_case "first" `Quick (fun () ->
            test_parse_digit_meta
              "467..114.."
              [ { value = 467; line_nr = 0; starts_at = 0; length = 3 }
              ; { value = 114; line_nr = 0; starts_at = 5; length = 3 }
              ])
        ; test_case "second" `Quick (fun () -> test_parse_digit_meta "...*......" [])
        ; test_case "third" `Quick (fun () ->
            test_parse_digit_meta
              "617*......"
              [ { value = 617; line_nr = 0; starts_at = 0; length = 3 } ])
        ] )
    ; ( "check_adjacent_poistions"
      , [ test_case "first" `Quick (fun () ->
            test_check_adjacent_positions
              { value = 114; line_nr = 0; starts_at = 5; length = 3 }
              [ "467..114.."
              ; "...*......"
              ; "..35..633."
              ; "......#..."
              ; "617*......"
              ; ".....+.58."
              ; "..592....."
              ; "......755."
              ; "...$.*...."
              ; ".664.598.."
              ]
              false)
        ; test_case "second" `Quick (fun () ->
            test_check_adjacent_positions
              { value = 633; line_nr = 2; starts_at = 6; length = 3 }
              [ "467..114.."
              ; "...*......"
              ; "..35..633."
              ; "......#..."
              ; "617*......"
              ; ".....+.58."
              ; "..592....."
              ; "......755."
              ; "...$.*...."
              ; ".664.598.."
              ]
              true)
        ] )
    ]
;;
