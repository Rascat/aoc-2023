let test_remove_letters_from_string input expected = 
  Alcotest.(check string) "same string" expected (Aoc_2023.Day01.remove_letters_from_string input)

let test_concat_first_and_last input expected = 
  Alcotest.(check string) "same string" expected (Aoc_2023.Day01.concat_first_and_last input)

let test_solve () = 
  let data = [
    "1abc2";
    "pqr3stu8vwx";
    "a1b2c3d4e5f";
    "treb7uchet";
  ] in
  Alcotest.(check int) "same int" 142 (Aoc_2023.Day01.solve data)


let () =
let open Alcotest in
run "Day 1" [
    "remove_letters_from_string", [
        test_case "first case" `Quick (fun () -> test_remove_letters_from_string "1abc2" "12");
        test_case "second case" `Quick (fun () -> test_remove_letters_from_string "pqr3stu8vwx" "38");
        test_case "third case" `Quick (fun () -> test_remove_letters_from_string "a1b2c3d4e5f" "12345");
        test_case "fourth case" `Quick (fun () -> test_remove_letters_from_string "treb7uchet" "7");
      ];
    "concat_first_and_last", [
      test_case "first case" `Quick (fun () -> test_concat_first_and_last "12" "12");
      test_case "second case" `Quick (fun () -> test_concat_first_and_last "38" "38");
      test_case "third case" `Quick (fun () -> test_concat_first_and_last "12345" "15");
      test_case "fourth case" `Quick (fun () -> test_concat_first_and_last "7" "77");
    ];
    "solve", [
      test_case "first case" `Quick (fun () -> test_solve ());
    ]
  ]