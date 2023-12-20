type flip_flop_state =
  { mutable input : bool
  ; mutable current : bool
  }

type conjunction_state = { inputs : (string, bool) Hashtbl.t }

type pulse_module =
  | FlipFlop of string * flip_flop_state * string list (* label, state, output*)
  | Conjunction of
      string * conjunction_state * string list (* label, state, output modules*)
  | Broadcaster of string * string list (* label, output labels *)

type counter =
  { mutable count_low : int
  ; mutable count_high : int
  }

type pulse =
  { source : string
  ; target : string
  ; is_high : bool
  }

let parse_module s =
  let open Re.Str in
  let re_module = regexp "\\([%&]\\)\\([a-z]+\\) -> \\([a-z]+\\(, [a-z]+\\)*\\)" in
  let re_bc = regexp "broadcaster -> \\([a-z]+\\(, [a-z]+\\)*\\)" in
  if string_match re_module s 0
  then (
    let type_sign = matched_group 1 s in
    let label = matched_group 2 s in
    let outputs = matched_group 3 s |> split (regexp ", ") in
    match type_sign with
    | "%" -> FlipFlop (label, { input = false; current = false }, outputs)
    | "&" -> Conjunction (label, { inputs = Hashtbl.create 10 }, outputs)
    | _ -> failwith ("Could not parse module with sign: " ^ type_sign))
  else if string_match re_bc s 0
  then (
    let outputs = matched_group 1 s |> split (regexp ", ") in
    Broadcaster ("broadcaster", outputs))
  else failwith "Could not parse module"
;;

let get_module l state = Hashtbl.find_opt state l

let get_inputs_of target_label state =
  List.fold_left
    (fun acc m ->
      match m with
      | FlipFlop (label, _, outputs) ->
        if List.mem target_label outputs then label :: acc else acc
      | Conjunction (label, _, outputs) ->
        if List.mem target_label outputs then label :: acc else acc
      | Broadcaster (label, outputs) ->
        if List.mem target_label outputs then label :: acc else acc)
    []
    state
;;

(*
   The cycle consists of the follwing steps:
   1. Given the current modules, for each determine whether to send a pulse or not,
   and update the inputs of all of their output modules accordingly
   2. For all the following modules, repeat
*)
let run_cycles module_state start_pulse counter =
  (* let counter = { count_low = 1; count_high = 0 } in *)
  counter.count_low <- counter.count_low + 1;
  let rec cycle current_pulses =
    List.iter
      (fun p -> print_endline (Printf.sprintf "%s -%b-> %s" p.source p.is_high p.target))
      current_pulses;
    let next_pulses = ref [] in
    match current_pulses with
    | [] -> ()
    | _ ->
      List.iter
        (fun { source; target; is_high } ->
          let target_mod = get_module target module_state in
          let pulse_opt =
            match target_mod with
            | Some (FlipFlop (_, ff_state, _)) ->
              if is_high
              then None
              else (
                ff_state.current <- not ff_state.current;
                Some ff_state.current)
            | Some (Conjunction (_, state, _)) ->
              Hashtbl.replace state.inputs source is_high;
              if Hashtbl.fold
                   (fun _ input_status acc -> input_status && acc)
                   state.inputs
                   true
              then Some false
              else Some true
            | Some (Broadcaster _) -> Some false
            | None -> None
          in
          let outputs =
            match target_mod with
            | Some (FlipFlop (_, _, outputs)) -> outputs
            | Some (Conjunction (_, _, outputs)) -> outputs
            | Some (Broadcaster (_, outputs)) -> outputs
            | None -> []
          in
          match pulse_opt with
          | Some pulse ->
            if pulse
            then counter.count_high <- counter.count_high + List.length outputs
            else counter.count_low <- counter.count_low + List.length outputs;
            let pulses_to_out =
              List.map (fun o -> { source = target; target = o; is_high = pulse }) outputs
            in
            next_pulses := !next_pulses @ pulses_to_out
          | None -> ())
        current_pulses;
      cycle !next_pulses
  in
  cycle [ start_pulse ]
;;

let solve_part_one data =
  let modules = List.map parse_module data in
  (* init combination inputs *)
  List.iter
    (fun m ->
      match m with
      | FlipFlop _ | Broadcaster _ -> ()
      | Conjunction (label, state, _) ->
        let inputs = get_inputs_of label modules in
        List.iter (fun i -> Hashtbl.add state.inputs i false) inputs)
    modules;
  (* create cuircuit state *)
  let module_state = Hashtbl.create 58 in
  List.iter
    (fun m ->
      match m with
      | FlipFlop (label, _, _) -> Hashtbl.add module_state label m
      | Conjunction (label, _, _) -> Hashtbl.add module_state label m
      | Broadcaster _ -> Hashtbl.add module_state "broadcaster" m)
    modules;
  let counter = { count_low = 0; count_high = 0 } in
  for _ = 1 to 1000 do
    run_cycles
      module_state
      { source = "button"; target = "broadcaster"; is_high = false }
      counter
  done;
  counter |> fun { count_high; count_low } -> count_high * count_low
;;
