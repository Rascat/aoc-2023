let test_remove_letters_from_string input expected =
  Alcotest.(check string)
    "same string"
    expected
    (Aoc_2023.Day01.remove_letters_from_string input)
;;

let test_concat_first_and_last input expected =
  Alcotest.(check string)
    "same string"
    expected
    (Aoc_2023.Day01.concat_first_and_last input)
;;

let test_replace_letters input expected =
  Alcotest.(check string)
    "same string"
    expected
    (Aoc_2023.Day01.replace_number_word_with_number input)
;;

let test_solve_part_one () =
  let data = [ "1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet" ] in
  Alcotest.(check int) "same int" 142 (Aoc_2023.Day01.solve_part_one data)
;;

let test_solve_part_two () =
  let data =
    [ "two1nine"
    ; "eightwothree"
    ; "abcone2threexyz"
    ; "xtwone3four"
    ; "4nineeightseven2"
    ; "zoneight234"
    ; "7pqrstsixteen"
    ]
  in
  Alcotest.(check int) "same int" 281 (Aoc_2023.Day01.solve_part_two data)
;;

let () =
  let open Alcotest in
  run
    "Day 1"
    [ ( "remove_letters_from_string"
      , [ test_case "first case" `Quick (fun () ->
            test_remove_letters_from_string "1abc2" "12")
        ; test_case "second case" `Quick (fun () ->
            test_remove_letters_from_string "pqr3stu8vwx" "38")
        ; test_case "third case" `Quick (fun () ->
            test_remove_letters_from_string "a1b2c3d4e5f" "12345")
        ; test_case "fourth case" `Quick (fun () ->
            test_remove_letters_from_string "treb7uchet" "7")
        ] )
    ; ( "concat_first_and_last"
      , [ test_case "first case" `Quick (fun () -> test_concat_first_and_last "12" "12")
        ; test_case "second case" `Quick (fun () -> test_concat_first_and_last "38" "38")
        ; test_case "third case" `Quick (fun () ->
            test_concat_first_and_last "12345" "15")
        ; test_case "fourth case" `Quick (fun () -> test_concat_first_and_last "7" "77")
        ] )
    ; ( "solve_part_one"
      , [ test_case "solve test input for part one" `Quick (fun () ->
            test_solve_part_one ())
        ] )
    ; ( "solve_part_two"
      , [ test_case "solve test input for part two" `Quick (fun () ->
            test_solve_part_two ())
        ] )
    ; ( "test_replace_letters"
      , [ test_case "first case" `Quick (fun () -> test_replace_letters "one" "1")
        ; test_case "second case" `Quick (fun () -> test_replace_letters "two" "2")
        ; test_case "third case" `Quick (fun () -> test_replace_letters "three" "3")
        ; test_case "fourth case" `Quick (fun () -> test_replace_letters "four" "4")
        ; test_case "fifth case" `Quick (fun () -> test_replace_letters "five" "5")
        ; test_case "sixth case" `Quick (fun () -> test_replace_letters "six" "6")
        ; test_case "seventh case" `Quick (fun () -> test_replace_letters "seven" "7")
        ; test_case "eighth case" `Quick (fun () -> test_replace_letters "eight" "8")
        ; test_case "ninth case" `Quick (fun () -> test_replace_letters "nine" "9")
        ; test_case "tenth case" `Quick (fun () -> test_replace_letters "two1nine" "219")
        ; test_case "eleventh case" `Quick (fun () ->
            test_replace_letters "eightwothree" "8wo3")
        ; test_case "twelth case" `Quick (fun () ->
            test_replace_letters "abcone2threexyz" "abc123xyz")
        ; test_case "thirteenth case" `Quick (fun () ->
            test_replace_letters "xtwone3four" "x2ne34")
        ; test_case "fourteenth case" `Quick (fun () ->
            test_replace_letters "4nineeightseven2" "49872")
        ; test_case "fifteenth case" `Quick (fun () ->
            test_replace_letters "zoneight234" "z1ight234")
        ; test_case "sixteenth case" `Quick (fun () ->
            test_replace_letters "4nineeightseven2" "49872")
        ; test_case "seventeenth case" `Quick (fun () ->
            test_replace_letters "7pqrstsixteen" "7pqrst6teen")
        ] )
    ]
;;
