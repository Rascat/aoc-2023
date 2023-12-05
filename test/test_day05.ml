open Aoc_2023.Day05

let test_data =
  [ "seeds: 79 14 55 13"
  ; ""
  ; "seed-to-soil map:"
  ; "50 98 2"
  ; "52 50 48"
  ; ""
  ; "soil-to-fertilizer map:"
  ; "0 15 37"
  ; "37 52 2"
  ; "39 0 15"
  ; ""
  ; "fertilizer-to-water map:"
  ; "49 53 8"
  ; "0 11 42"
  ; "42 0 7"
  ; "57 7 4"
  ; ""
  ; "water-to-light map:"
  ; "88 18 7"
  ; "18 25 70"
  ; ""
  ; "light-to-temperature map:"
  ; "45 77 23"
  ; "81 45 19"
  ; "68 64 13"
  ; ""
  ; "temperature-to-humidity map:"
  ; "0 69 1"
  ; "1 0 69"
  ; ""
  ; "humidity-to-location map:"
  ; "60 56 37"
  ; "56 93 4"
  ]
;;

let test_parse_maps input expected =
  let actual = parse_maps input in
  (* print maps *)
  List.iter
    (fun map ->
      print_endline map.name;
      List.iter
        (fun range ->
          Printf.printf
            "  dest: %d | src: %d (%d)\n"
            range.destination_range_start
            range.source_range_start
            range.length)
        map.ranges)
    actual;
  (* check maps *)
  List.iter2
    (fun expected_map actual_map ->
      Alcotest.(check string) "same name" expected_map.name actual_map.name;
      List.iter2
        (fun expected_range actual_range ->
          Alcotest.(check int)
            ("same destination range start in map " ^ expected_map.name)
            expected_range.destination_range_start
            actual_range.destination_range_start;
          Alcotest.(check int)
            ("same source range start in map " ^ expected_map.name)
            expected_range.source_range_start
            actual_range.source_range_start;
          Alcotest.(check int) "same length" expected_range.length actual_range.length)
        expected_map.ranges
        actual_map.ranges)
    expected
    actual
;;

let test_solve_part_one input expected =
  let actual = solve_part_one input in
  Alcotest.(check int) "same result for part one" expected actual
;;

let test_solve_part_two input expected =
  let actual = solve_part_two input in
  Alcotest.(check int) "same result for part two" expected actual
;;

let () =
  let open Alcotest in
  run
    "Day05"
    [ ( "parse_maps"
      , [ test_case "parse maps" `Quick (fun () ->
            let input =
              [ "seed-to-soil map:"
              ; "50 98 2"
              ; "52 50 48"
              ; ""
              ; "soil-to-fertilizer map:"
              ; "0 15 37"
              ; "37 52 2"
              ; "39 0 15"
              ; ""
              ; "fertilizer-to-water map:"
              ; "49 53 8"
              ; "0 11 42"
              ; "42 0 7"
              ; "57 7 4"
              ]
            in
            let expected =
              [ { name = "seed-to-soil"
                ; ranges =
                    [ { destination_range_start = 50
                      ; source_range_start = 98
                      ; length = 2
                      }
                    ; { destination_range_start = 52
                      ; source_range_start = 50
                      ; length = 48
                      }
                    ]
                }
              ; { name = "soil-to-fertilizer"
                ; ranges =
                    [ { destination_range_start = 0
                      ; source_range_start = 15
                      ; length = 37
                      }
                    ; { destination_range_start = 37
                      ; source_range_start = 52
                      ; length = 2
                      }
                    ; { destination_range_start = 39
                      ; source_range_start = 0
                      ; length = 15
                      }
                    ]
                }
              ; { name = "fertilizer-to-water"
                ; ranges =
                    [ { destination_range_start = 49
                      ; source_range_start = 53
                      ; length = 8
                      }
                    ; { destination_range_start = 0
                      ; source_range_start = 11
                      ; length = 42
                      }
                    ; { destination_range_start = 42; source_range_start = 0; length = 7 }
                    ; { destination_range_start = 57; source_range_start = 7; length = 4 }
                    ]
                }
              ]
            in
            test_parse_maps input expected)
        ] )
    ; ( "solve_part_one"
      , [ test_case "solve part one" `Quick (fun () -> test_solve_part_one test_data 35) ]
      )
    ; ( "solve_part_two"
      , [ test_case "solve part two" `Quick (fun () -> test_solve_part_two test_data 46) ]
      )
    ]
;;
