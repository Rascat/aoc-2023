let () =
let filename = "inputs/day01.txt" in
let data = Aoc_2023.Utils.read_lines filename in
 Printf.printf "Solution to Day 1.1: %i\n" (Aoc_2023.Day01.solve data)
