type node =
  { id : string
  ; left : string
  ; right : string
  }

let parse_node l =
  Scanf.sscanf l "%s = (%s@, %s@)" (fun id left right -> { id; left; right })
;;

let parse_directions s = Utils.string_to_char_list s

let navigate_map directions nodes =
  let rec navigate_map' curr_directions curr_node count =
    if curr_node.id = "ZZZ"
    then count
    else (
      match curr_directions with
      | [] -> navigate_map' directions curr_node count
      | d :: rest ->
        let next_node =
          if d = 'L'
          then List.find (fun n -> n.id = curr_node.left) nodes
          else List.find (fun n -> n.id = curr_node.right) nodes
        in
        navigate_map' rest next_node (count + 1))
  in
  navigate_map' directions (List.find (fun n -> n.id = "AAA") nodes) 0
;;

let solve_part_one data =
  let directions = parse_directions (List.hd data) in
  let nodes = List.tl (List.tl data) |> List.map parse_node in
  navigate_map directions nodes
;;

let solve_part_two _data = failwith "Not implemented"
