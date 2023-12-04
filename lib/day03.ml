type digit_meta =
  { value : int
  ; line_nr : int
  ; starts_at : int
  ; length : int
  }

type env =
  { line : string
  ; line_nr : int
  }

let parse_digit_meta line line_nr =
  let open Re.Str in
  let re_digits = regexp "\\([0-9]+\\)" in
  let rec parse_digit_meta' starts_at (env : env) =
    match search_forward re_digits env.line starts_at with
    | exception Not_found -> []
    | start ->
      let s = matched_string env.line in
      let value = int_of_string s in
      let length = String.length s in
      { value; line_nr = env.line_nr; starts_at = start; length }
      :: parse_digit_meta' (start + length) env
  in
  parse_digit_meta' 0 { line; line_nr }
;;

let string_at_pos x y (data : string list) =
  match List.nth data y with
  | exception _ -> "."
  | line ->
    (match String.get line x with
     | exception Invalid_argument _ -> "."
     | c -> Utils.string_of_char c)
;;

let check_adjacent_positions digit_meta data =
  let open Re.Str in
  let re_symbol = regexp "[^0-9.]" in
  let check_left_end =
    let x = digit_meta.starts_at in
    let y = digit_meta.line_nr in
    string_match re_symbol (string_at_pos (x - 1) (y - 1) data) 0
    || string_match re_symbol (string_at_pos x (y - 1) data) 0
    || string_match re_symbol (string_at_pos (x - 1) y data) 0
    || string_match re_symbol (string_at_pos (x - 1) (y + 1) data) 0
    || string_match re_symbol (string_at_pos x (y + 1) data) 0
  in
  let check_right_end =
    let x = digit_meta.starts_at + digit_meta.length - 1 in
    let y = digit_meta.line_nr in
    string_match re_symbol (string_at_pos x (y - 1) data) 0
    || string_match re_symbol (string_at_pos (x + 1) (y - 1) data) 0
    || string_match re_symbol (string_at_pos (x + 1) y data) 0
    || string_match re_symbol (string_at_pos x (y + 1) data) 0
    || string_match re_symbol (string_at_pos (x + 1) (y + 1) data) 0
  in
  let check_above_below x y =
    string_match re_symbol (string_at_pos x (y - 1) data) 0
    || string_match re_symbol (string_at_pos x (y + 1) data) 0
  in
  let rec check_neighbors index =
    if index == 0
    then (
      match check_left_end with
      | true -> true
      | false -> check_neighbors (index + 1))
    else if index < digit_meta.length - 1
    then (
      match check_above_below (digit_meta.starts_at + index) digit_meta.line_nr with
      | true -> true
      | false -> check_neighbors (index + 1))
    else check_right_end
  in
  check_neighbors 0
;;

let solve_part_one data =
  List.mapi (fun line_nr line -> parse_digit_meta line line_nr) data
  |> List.flatten
  |> List.filter (fun digit_meta -> check_adjacent_positions digit_meta data)
  |> List.map (fun digit_meta -> digit_meta.value)
  |> List.fold_left ( + ) 0
;;

let solve_part_two _data = failwith "Not implemented yet"
