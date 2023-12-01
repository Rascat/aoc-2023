let is_number c = c >= '0' && c <= '9'

let remove_letters_from_string s = (Utils.char_list_to_string (List.filter (is_number) (Utils.string_to_char_list s)))

let concat_first_and_last s =
  let first = String.get s 0 in
  let last = String.get s ((String.length s) - 1) in
  (String.make 1 first) ^ (String.make 1 last)

let solve data =
  let data_clean = List.map remove_letters_from_string data in
  let numbers = data_clean |> List.map concat_first_and_last |> List.map int_of_string in
  List.fold_left (+) 0 numbers
