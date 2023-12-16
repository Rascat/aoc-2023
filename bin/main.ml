open Cmdliner

let aoc_2023 day part file =
  let data = Aoc_2023.Utils.read_lines file in
  let solver =
    match day, part with
    | 1, 1 -> Some Aoc_2023.Day01.solve_part_one
    | 1, 2 -> Some Aoc_2023.Day01.solve_part_two
    | 2, 1 -> Some Aoc_2023.Day02.solve_part_one
    | 2, 2 -> Some Aoc_2023.Day02.solve_part_two
    | 3, 1 -> Some Aoc_2023.Day03.solve_part_one
    | 3, 2 -> Some Aoc_2023.Day03.solve_part_two
    | 4, 1 -> Some Aoc_2023.Day04.solve_part_one
    | 4, 2 -> Some Aoc_2023.Day04.solve_part_two
    | 5, 1 -> Some Aoc_2023.Day05.solve_part_one
    | 5, 2 -> Some Aoc_2023.Day05.solve_part_two
    | 6, 1 -> Some Aoc_2023.Day06.solve_part_one
    | 6, 2 -> Some Aoc_2023.Day06.solve_part_two
    | 7, 1 -> Some Aoc_2023.Day07.solve_part_one
    | 7, 2 -> Some Aoc_2023.Day07.solve_part_two
    | 8, 1 -> Some Aoc_2023.Day08.solve_part_one
    | 8, 2 -> Some Aoc_2023.Day08.solve_part_two
    | 9, 1 -> Some Aoc_2023.Day09.solve_part_one
    | 9, 2 -> Some Aoc_2023.Day09.solve_part_two
    | 10, 1 -> Some Aoc_2023.Day10.solve_part_one
    | 11, 1 -> Some Aoc_2023.Day11.solve_part_one
    | 11, 2 -> Some Aoc_2023.Day11.solve_part_two
    | 13, 1 -> Some Aoc_2023.Day13.solve_part_one
    | 14, 1 -> Some Aoc_2023.Day14.solve_part_one
    | 15, 1 -> Some Aoc_2023.Day15.solve_part_one
    | 15, 2 -> Some Aoc_2023.Day15.solve_part_two
    | 16, 1 -> Some Aoc_2023.Day16.solve_part_one
    | 16, 2 -> Some Aoc_2023.Day16.solve_part_two
    | _ -> None
  in
  match solver with
  | Some fn -> print_endline (Printf.sprintf "Day %d.%d: %d" day part (fn data))
  | None -> print_endline (Printf.sprintf "No solution for day %d.%d" day part)
;;

let day =
  let doc = "Specifies the day for which the solution is computed (1-24)." in
  Arg.(required & pos 0 (some int) None & info [] ~docv:"DAY" ~doc)
;;

let part =
  let doc = "Specifies the part for which the solution is computed (1 or 2)." in
  Arg.(required & pos 1 (some int) None & info [] ~docv:"PART" ~doc)
;;

let file =
  let doc = "Specifies the path to the input file" in
  Arg.(required & pos 2 (some string) None & info [] ~docv:"FILE" ~doc)
;;

let solve_aoc_t = Term.(const aoc_2023 $ day $ part $ file)

let cmd =
  let doc = "compute solution for any of 2023's Advent of Code challenges" in
  let info = Cmd.info "aoc_2023" ~doc in
  Cmd.v info solve_aoc_t
;;

let main () = exit (Cmd.eval cmd)
let () = main ()
