type lens =
  { label : string
  ; focal_length : int
  }

type action =
  | Remove
  | Add of int

type operation =
  { label : string
  ; action : action
  }

let parse_operation s =
  if String.contains s '='
  then (
    let parts = String.split_on_char '=' s in
    { label = List.nth parts 0; action = Add (int_of_string (List.nth parts 1)) })
  else (
    let parts = String.split_on_char '-' s in
    { label = List.hd parts; action = Remove })
;;

let compute_hash s =
  let chars = Utils.string_to_char_list s in
  let mod_256 x = x mod 256 in
  List.fold_left (fun acc c -> acc + Char.code c |> ( * ) 17 |> mod_256) 0 chars
;;

let solve_part_one data =
  let sequence = List.hd data |> String.split_on_char ',' in
  sequence |> List.map compute_hash |> Utils.sum
;;

let solve_part_two data =
  let lens_table = Hashtbl.create 256 in
  let operations = List.hd data |> String.split_on_char ',' |> List.map parse_operation in
  List.iter
    (fun o ->
      let box = compute_hash o.label in
      match o.action with
      | Add x ->
        (match Hashtbl.find_opt lens_table box with
         | None -> Hashtbl.add lens_table box [ { label = o.label; focal_length = x } ]
         | Some lenses ->
           (* Look if the lens is already in the box. if yes -> update, if no -> add to back*)
           (match List.find_opt (fun (l : lens) -> l.label = o.label) lenses with
            | None ->
              Hashtbl.replace
                lens_table
                box
                (lenses @ [ { label = o.label; focal_length = x } ])
            | Some _ ->
              Hashtbl.replace
                lens_table
                box
                (List.map
                   (fun (l : lens) ->
                     if l.label <> o.label then l else { l with focal_length = x })
                   lenses)))
      | Remove ->
        (match Hashtbl.find_opt lens_table box with
         | None -> (* do nothing *) ()
         | Some lenses ->
           let filtered_lenses : lens list =
             List.filter (fun (l : lens) -> l.label <> o.label) lenses
           in
           Hashtbl.replace lens_table box filtered_lenses))
    operations;
  Hashtbl.fold
    (fun box lenses acc ->
      acc
      + (List.mapi (fun i l -> (box + 1) * (i + 1) * l.focal_length) lenses |> Utils.sum))
    lens_table
    0
;;
