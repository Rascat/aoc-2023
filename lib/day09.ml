let parse_history l = Utils.find_digits l

let differences numbers =
  let rec differences' acc = function
    | [] -> acc
    | _ :: [] -> acc
    | a :: b :: rest -> differences' ((b - a) :: acc) (b :: rest)
  in
  differences' [] numbers |> List.rev
;;

let generate_sequences numbers =
  let rec generate acc curr_numbers =
    if List.for_all (fun n -> n = 0) curr_numbers
    then curr_numbers :: acc |> List.rev
    else generate (curr_numbers :: acc) (differences curr_numbers)
  in
  generate [] numbers
;;

let extrapolate_sequences sequences =
  let rec aux acc s =
    match s with
    | [] -> failwith "should not happen"
    | a :: rest ->
      if List.for_all (fun d -> d = 0) a
      then [ 0 ]
      else (
        let last_nr = List.rev a |> List.hd in
        let exs = aux acc rest in
        (last_nr + List.hd exs) :: exs)
  in
  aux [] sequences |> List.rev
;;

let solve_part_one data =
  let histories = List.map (fun l -> parse_history l) data in
  let extrapolated_values =
    List.map
      (fun h -> generate_sequences h |> extrapolate_sequences |> List.rev |> List.hd)
      histories
  in
  List.fold_left ( + ) 0 extrapolated_values
;;

let solve_part_two _data = failwith "Not implemented yet"
