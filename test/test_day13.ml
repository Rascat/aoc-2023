open Aoc_2023.Day13

let test_data =
  [ "#.##..##."
  ; "..#.##.#."
  ; "##......#"
  ; "##......#"
  ; "..#.##.#."
  ; "..##..##."
  ; "#.#.##.#."
  ; ""
  ; "#...##..#"
  ; "#....#..#"
  ; "..##..###"
  ; "#####.##."
  ; "#####.##."
  ; "..##..###"
  ; "#....#..#"
  ]
;;

let _block_1 =
  [ "#.##..##."
  ; "..#.##.#."
  ; "##......#"
  ; "##......#"
  ; "..#.##.#."
  ; "..##..##."
  ; "#.#.##.#."
  ]
;;

let block_2 =
  [ "#...##..#"
  ; "#....#..#"
  ; "..##..###"
  ; "#####.##."
  ; "#####.##."
  ; "..##..###"
  ; "#....#..#"
  ]
;;

let test_parse_block () =
  let result = parse_blocks test_data in
  Alcotest.(check int) "same length" 2 (List.length result);
  Alcotest.(check string)
    "same first line on block #1"
    (List.hd test_data)
    (List.hd (List.hd result));
  Alcotest.(check string)
    "same last line on block #2"
    (List.hd (List.rev test_data))
    (List.hd (List.rev (List.nth result 1)))
;;

let test_find_horizontal_axis input expected =
  let result = find_horizontal_axis input in
  match result with
  | Some i -> Alcotest.(check int) "same index" expected i
  | None -> Alcotest.fail "Should find an axis"
;;

let () =
  let open Alcotest in
  run
    "Day13"
    [ "test_parse_block", [ test_case "#1" `Quick test_parse_block ]
    ; ( "test_find_horizontal_axis"
      , [ test_case "#1" `Quick (fun () -> test_find_horizontal_axis block_2 4) ] )
    ]
;;
