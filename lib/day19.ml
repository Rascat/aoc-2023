type part =
  { x : int
  ; m : int
  ; a : int
  ; s : int
  }

type step =
  | Default of string
  | Condition of (part -> int) * (int -> bool) * string

let parse_blocks data =
  let rec parse' block_acc list_acc = function
    | [] -> List.rev (List.rev block_acc :: list_acc)
    | h :: rest ->
      if h = ""
      then parse' [] (List.rev block_acc :: list_acc) rest
      else parse' (h :: block_acc) list_acc rest
  in
  parse' [] [] data
;;

let parse_part s = Scanf.sscanf s "{x=%d,m=%d,a=%d,s=%d}" (fun x m a s -> { x; m; a; s })

let parse_step s =
  let open Re.Str in
  let step_re = regexp "\\([xmas]\\)\\([<>]\\)\\([0-9]+\\):\\([AR]\\|[a-z]+\\)" in
  if string_match step_re s 0
  then (
    let category = String.get (matched_group 1 s) 0 in
    let comparison_operator = String.get (matched_group 2 s) 0 in
    let compared_value = matched_group 3 s |> int_of_string in
    let target = matched_group 4 s in
    let category_selector =
      match category with
      | 'x' ->
        fun (part : part) ->
          (match part with
           | { x; m = _; a = _; s = _ } -> x)
      | 'm' ->
        fun part ->
          (match part with
           | { x = _; m; a = _; s = _ } -> m)
      | 'a' ->
        fun part ->
          (match part with
           | { x = _; m = _; a; s = _ } -> a)
      | 's' ->
        fun part ->
          (match part with
           | { x = _; m = _; a = _; s } -> s)
      | _ -> failwith ("Could not parse category" ^ String.make 1 category)
    in
    match comparison_operator with
    | '>' -> Condition (category_selector, (fun d -> d > compared_value), target)
    | '<' -> Condition (category_selector, (fun d -> d < compared_value), target)
    | _ ->
      failwith
        ("Could not match comparison operator: " ^ String.make 1 comparison_operator))
  else Default s
;;

let parse_workflow s =
  let label, step_data = Scanf.sscanf s "%s@{%s@}" (fun lbl steps -> lbl, steps) in
  let steps = String.split_on_char ',' step_data |> List.map parse_step in
  label, steps
;;

let is_accepted part workflows start_label =
  let rec aux steps =
    match steps with
    | Condition (category_selector, predicate, target) :: rest ->
      if predicate (category_selector part)
      then (
        match target with
        | "A" -> true
        | "R" -> false
        | lbl -> aux (List.assoc lbl workflows))
      else aux rest
    | Default target :: [] ->
      (match target with
       | "A" -> true
       | "R" -> false
       | lbl -> aux (List.assoc lbl workflows))
    | _ -> failwith "Something went wrong"
  in
  aux (List.assoc start_label workflows)
;;

let add_ratings { x; m; a; s } = x + m + a + s 

let solve_part_one data =
  let blocks = parse_blocks data in
  let workflows = List.nth blocks 0 |> List.map parse_workflow in
  let parts = List.nth blocks 1 |> List.map parse_part in
  List.filter (fun p -> is_accepted p workflows "in") parts |> List.map add_ratings |> Utils.sum
;;
