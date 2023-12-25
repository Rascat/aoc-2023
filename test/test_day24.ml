open Aoc_2023.Day24

let test_line_intersection_2d input expected =
  Alcotest.(check (option (pair (float 0.3) (float 0.3))))
    "same intersect"
    expected
    (line_intersection_2d input)
;;

let () =
  let open Alcotest in
  run
    "Day24"
    [ ( "test line_intersection_2d"
      , [ test_case "intersect" `Quick (fun () ->
            test_line_intersection_2d
              (((19, 13), (17, 14)), ((18, 19), (17, 18)))
              (Some (14.333, 15.333)))
        ; test_case "parallel" `Quick (fun () ->
            test_line_intersection_2d (((18, 19), (17, 18)), ((20, 25), (18, 23))) None)
        ; test_case "#3" `Quick (fun () ->
            test_line_intersection_2d
              (((19, 13), (17, 14)), ((20, 19), (21, 14)))
              (Some (21.444, 11.777)))
          (*
             21.444 - 19 = 2.444
             11.777 - 13 = -1.223
             ====================
             21.444 - 20 = 1.444
             11.777 - 19 = -7.233
          *)
        ] )
    ]
;;
