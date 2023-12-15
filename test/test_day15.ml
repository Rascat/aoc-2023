open Aoc_2023.Day15

let test_compute_hash input expected =
  Alcotest.(check int) "same hash" expected (compute_hash input)
;;

let () =
  let open Alcotest in
  run
    "Day15"
    [ ( "test_compute_hash"
      , [ test_case "#1" `Quick (fun () -> test_compute_hash "rn=1" 30)
        ; test_case "#2" `Quick (fun () -> test_compute_hash "cm-" 253)
        ; test_case "#3" `Quick (fun () -> test_compute_hash "qp=3" 97)
        ; test_case "#4" `Quick (fun () -> test_compute_hash "cm=2" 47)
        ; test_case "#5" `Quick (fun () -> test_compute_hash "rn" 0)
        ; test_case "#6" `Quick (fun () -> test_compute_hash "cm" 0)
        ] )
    ]
;;
