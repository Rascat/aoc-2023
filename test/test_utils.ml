let test_string_to_char_list input expected =
  Alcotest.(check (list char))
    "same lists"
    expected
    (Aoc_2023.Utils.string_to_char_list input)
;;

let test_char_list_to_string input expected =
  Alcotest.(check string)
    "same string"
    expected
    (Aoc_2023.Utils.char_list_to_string input)
;;

let () =
  let open Alcotest in
  run
    "Utils"
    [ ( "string_to_char_list"
      , [ test_case "empty" `Quick (fun () -> test_string_to_char_list "" [])
        ; test_case "standard" `Quick (fun () ->
            test_string_to_char_list "love" [ 'l'; 'o'; 'v'; 'e' ])
        ] )
    ; ( "char_list_to_string"
      , [ test_case "empty" `Quick (fun () -> test_char_list_to_string [] "")
        ; test_case "standard" `Quick (fun () ->
            test_char_list_to_string [ 'l'; 'o'; 'v'; 'e' ] "love")
        ] )
    ]
;;
