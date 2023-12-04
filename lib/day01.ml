let is_number c = c >= '0' && c <= '9'

let remove_letters_from_string s =
  Utils.char_list_to_string (List.filter is_number (Utils.string_to_char_list s))
;;

let rec replace_number_word_with_number s =
  let open Re.Str in
  let number_regex =
    regexp "one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine"
  in
  match search_forward number_regex s 0 with
  | exception Not_found -> s
  | _ ->
    (match matched_string s with
     | "one" -> replace_first number_regex "1" s |> replace_number_word_with_number
     | "two" -> replace_first number_regex "2" s |> replace_number_word_with_number
     | "three" -> replace_first number_regex "3" s |> replace_number_word_with_number
     | "four" -> replace_first number_regex "4" s |> replace_number_word_with_number
     | "five" -> replace_first number_regex "5" s |> replace_number_word_with_number
     | "six" -> replace_first number_regex "6" s |> replace_number_word_with_number
     | "seven" -> replace_first number_regex "7" s |> replace_number_word_with_number
     | "eight" -> replace_first number_regex "8" s |> replace_number_word_with_number
     | "nine" -> replace_first number_regex "9" s |> replace_number_word_with_number
     | _ -> failwith "This should never happen")
;;

let concat_first_and_last s =
  let first = String.get s 0 in
  let last = String.get s (String.length s - 1) in
  String.make 1 first ^ String.make 1 last
;;

let solve_part_one data =
  List.map remove_letters_from_string data
  |> List.map concat_first_and_last
  |> List.map int_of_string
  |> Utils.sum
;;

let solve_part_two data =
  List.map replace_number_word_with_number data
  |> List.map remove_letters_from_string
  |> List.map concat_first_and_last
  |> List.map int_of_string
  |> Utils.sum
;;
