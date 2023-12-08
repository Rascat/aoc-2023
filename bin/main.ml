let () =
  let d1_data = Aoc_2023.Utils.read_lines "inputs/day01.txt" in
  let d2_data = Aoc_2023.Utils.read_lines "inputs/day02.txt" in
  let d3_data = Aoc_2023.Utils.read_lines "inputs/day03.txt" in
  let d4_data = Aoc_2023.Utils.read_lines "inputs/day04.txt" in
  let d5_data = Aoc_2023.Utils.read_lines "inputs/day05.txt" in
  let d6_data = Aoc_2023.Utils.read_lines "inputs/day06.txt" in
  let d7_data = Aoc_2023.Utils.read_lines "inputs/day07.txt" in
  let d8_data = Aoc_2023.Utils.read_lines "inputs/day08.txt" in
  Printf.printf "Solution to Day 1.1: %i\n" (Aoc_2023.Day01.solve_part_one d1_data);
  Printf.printf "Solution to Day 1.2: %i\n" (Aoc_2023.Day01.solve_part_two d1_data);
  Printf.printf "Solution to Day 2.1: %i\n" (Aoc_2023.Day02.solve_part_one d2_data);
  Printf.printf "Solution to Day 2.2: %i\n" (Aoc_2023.Day02.solve_part_two d2_data);
  Printf.printf "Solution to Day 3.1: %i\n" (Aoc_2023.Day03.solve_part_one d3_data);
  Printf.printf "Solution to Day 4.1: %i\n" (Aoc_2023.Day04.solve_part_one d4_data);
  Printf.printf "Solution to Day 4.2: %i\n" (Aoc_2023.Day04.solve_part_two d4_data);
  Printf.printf "Solution to Day 5.1: %i\n" (Aoc_2023.Day05.solve_part_one d5_data);
  Printf.printf "Solution to Day 5.2: %i\n" (Aoc_2023.Day05.solve_part_two_seq d5_data);
  Printf.printf "Solution to Day 6.1: %i\n" (Aoc_2023.Day06.solve_part_one d6_data);
  Printf.printf "Solution to Day 6.2: %i\n" (Aoc_2023.Day06.solve_part_two d6_data);
  Printf.printf "Solution to Day 7.1: %i\n" (Aoc_2023.Day07.solve_part_one d7_data);
  Printf.printf "Solution to Day 8.1: %i\n" (Aoc_2023.Day08.solve_part_one d8_data)
;;
