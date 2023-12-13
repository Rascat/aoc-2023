let split_at_index index lst =
  let rec split_aux i acc = function
    | [] -> List.rev acc, []
    | x :: xs as l -> if i = 0 then List.rev acc, l else split_aux (i - 1) (x :: acc) xs
  in
  split_aux index [] lst
;;

let find_horizontal_axis _block = failwith "Not implemented"
let solve_part_one _data = failwith "Not implemented yet"
let solve_part_two _data = failwith "Not implemented yet"
