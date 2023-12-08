type node =
  { id : string
  ; left : string
  ; right : string
  }

let parse_node l =
  Scanf.sscanf l "%s = (%s@, %s@)" (fun id left right -> { id; left; right })
;;

let parse_directions s = Utils.string_to_char_list s

let navigate_map directions nodes start_nodes stop_on =
  let nodes_table = Hashtbl.create 1000 in
  List.iter (fun n -> Hashtbl.add nodes_table n.id n) nodes;
  let rec navigate_map' curr_directions curr_nodes count =
    if List.for_all stop_on curr_nodes
    then count
    else (
      match curr_directions with
      | [] -> navigate_map' directions curr_nodes count
      | d :: rest ->
        let next_nodes =
          if d = 'L'
          then List.map (fun cn -> Hashtbl.find nodes_table cn.left) curr_nodes
          else List.map (fun cn -> Hashtbl.find nodes_table cn.right) curr_nodes
        in
        navigate_map' rest next_nodes (count + 1))
  in
  navigate_map' directions start_nodes 0
;;

let walk_loop directions nodes start_node =
  let nodes_table = Hashtbl.create 1000 in
  List.iter (fun n -> Hashtbl.add nodes_table n.id n) nodes;
  let rec walk_loop' curr_directions curr_node count =
    if String.ends_with ~suffix:"Z" curr_node.id
    then count
    else (
      match curr_directions with
      | [] -> walk_loop' directions curr_node count
      | d :: rest ->
        let next_node =
          if d = 'L'
          then Hashtbl.find nodes_table curr_node.left
          else Hashtbl.find nodes_table curr_node.right
        in
        if String.ends_with next_node.id ~suffix:"Z"
        then
          print_endline
            (Printf.sprintf "=== (%s)-[%c]->(%s) ===" curr_node.id d next_node.id)
        else print_endline (Printf.sprintf "(%s)-[%c]->(%s)" curr_node.id d next_node.id);
        walk_loop' rest next_node (count + 1))
  in
  walk_loop' directions start_node 0
;;

let solve_part_one data =
  let directions = parse_directions (List.hd data) in
  let nodes = List.tl (List.tl data) |> List.map parse_node in
  let start_nodes = List.find_all (fun n -> n.id = "AAA") nodes in
  let stop_on n = n.id = "ZZZ" in
  print_endline (Printf.sprintf "%d " (List.length start_nodes));
  navigate_map directions nodes start_nodes stop_on
;;

let solve_part_two data =
  let directions = parse_directions (List.hd data) in
  let nodes = List.tl (List.tl data) |> List.map parse_node in
  let start_nodes = List.find_all (fun n -> String.ends_with ~suffix:"A" n.id) nodes in
  let stop_on n = String.ends_with ~suffix:"Z" n.id in
  navigate_map directions nodes start_nodes stop_on
;;

let find_circles data =
  let directions = parse_directions (List.hd data) in
  let nodes = List.tl (List.tl data) |> List.map parse_node in
  let start_nodes = List.find_all (fun n -> String.ends_with ~suffix:"A" n.id) nodes in
  let circle_sizes = List.map (fun n -> walk_loop directions nodes n) start_nodes in
  List.iter (fun s -> Printf.printf "%d " s) circle_sizes
;;
