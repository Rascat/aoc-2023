type range =
  { destination_range_start : int
  ; source_range_start : int
  ; length : int
  }

type map =
  { name : string
  ; ranges : range list
  }

let parse_maps input =
  let is_new_map line = String.contains line ':' in
  let is_range line =
    Re.Str.string_match (Re.Str.regexp "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)") line 0
  in
  let is_empty line = String.length line = 0 in
  let rec parse_map' curr_map acc lines =
    match lines with
    | [] ->
      let curr_map = { curr_map with ranges = List.rev curr_map.ranges } in
      curr_map :: acc
    | line :: lines ->
      if is_new_map line
      then (
        let name = Scanf.sscanf line "%s map:" (fun s -> s) in
        let curr_map = { name; ranges = [] } in
        parse_map' curr_map acc lines)
      else if is_range line
      then (
        let destination_range_start = Re.Str.matched_group 1 line |> int_of_string in
        let source_range_start = Re.Str.matched_group 2 line |> int_of_string in
        let length = Re.Str.matched_group 3 line |> int_of_string in
        let range = { destination_range_start; source_range_start; length } in
        let curr_map = { curr_map with ranges = range :: curr_map.ranges } in
        parse_map' curr_map acc lines)
      else if is_empty line
      then (
        let curr_map = { curr_map with ranges = List.rev curr_map.ranges } in
        parse_map' curr_map (curr_map :: acc) lines)
      else failwith "Unexpected input"
  in
  parse_map' { name = ""; ranges = [] } [] input |> List.rev
;;

let translate_number number ~map =
  let range =
    List.find_opt
      (fun r ->
        r.source_range_start <= number && number < r.source_range_start + r.length)
      map.ranges
  in
  match range with
  | None -> number
  | Some range ->
    let offset = number - range.source_range_start in
    range.destination_range_start + offset
;;

let translate_seed_to_soil seed_nr maps =
  seed_nr
  |> translate_number ~map:(List.find (fun m -> m.name = "seed-to-soil") maps)
  |> translate_number ~map:(List.find (fun m -> m.name = "soil-to-fertilizer") maps)
  |> translate_number ~map:(List.find (fun m -> m.name = "fertilizer-to-water") maps)
  |> translate_number ~map:(List.find (fun m -> m.name = "water-to-light") maps)
  |> translate_number ~map:(List.find (fun m -> m.name = "light-to-temperature") maps)
  |> translate_number ~map:(List.find (fun m -> m.name = "temperature-to-humidity") maps)
  |> translate_number ~map:(List.find (fun m -> m.name = "humidity-to-location") maps)
;;

let parse_input input =
  let seeds =
    List.hd input
    |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "seeds:")
    |> List.map int_of_string
  in
  let maps = parse_maps (List.tl (List.tl input)) in
  seeds, maps
;;

(* takes a starting number and a length and returns a list of all numbers from start to start + length - 1 *)
let rec range start length =
  if length = 0 then [] else start :: range (start + 1) (length - 1)
;;

(* tail recursive version of range *)
let range2 start length =
  let rec range' start length acc =
    if length = 0 then acc else range' (start + 1) (length - 1) (start :: acc)
  in
  range' start length []
;;

(* transform list of ints into list of pairs *)
let rec pairs = function
  | [] -> []
  | [ _ ] -> []
  | x :: y :: rest -> (x, y) :: pairs rest
;;

let solve_part_one data =
  let seeds, maps = parse_input data in
  let soil = List.map (fun seed -> translate_seed_to_soil seed maps) seeds in
  (* min of the soil numbers *)
  List.fold_left min (List.hd soil) (List.tl soil)
;;

let solve_part_two data =
  let seeds, maps = parse_input data in
  let pairs = pairs seeds in
  let ranges = List.map (fun (a, b) -> range2 a b) pairs in
  let ranges_flat = List.flatten ranges in
  let soil = List.map (fun seed -> translate_seed_to_soil seed maps) ranges_flat in
  (* min of the soil numbers *)
  List.fold_left min (List.hd soil) (List.tl soil)
;;
