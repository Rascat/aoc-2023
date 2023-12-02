let () =
  let d1_data = Aoc_2023.Utils.read_lines "inputs/day01.txt" in
  let d2_data = Aoc_2023.Utils.read_lines "inputs/day02.txt" in
  Printf.printf "Solution to Day 1.1: %i\n" (Aoc_2023.Day01.solve_part_one d1_data);
  Printf.printf "Solution to Day 1.2: %i\n" (Aoc_2023.Day01.solve_part_two d1_data);
  Printf.printf "Solution to Day 2.1: %i\n" (Aoc_2023.Day02.solve_part_one d2_data)
;;
