let section_separator section = ("\n" ^ section ^ ":", Arg.Unit ignore, " ")

let specs = ref [section_separator "help"]
let actions = ref []

let exe = Sys.argv.(0)


let register ?action section spec =
  specs := section_separator section :: spec @ !specs;
  match action with
  | None -> ()
  | Some action ->
      actions := action :: !actions


let run ?(args=[]) f =
  Printexc.record_backtrace true;

  let specs =
    List.map (fun (arg, kind, desc) ->
      let default =
        match kind with
        (* pass these on *)
        | Arg.Unit _ -> None

        | Arg.Set ref -> Some (string_of_bool !ref)
        | Arg.Set_int ref -> Some (string_of_int !ref)
        | Arg.Set_string ref -> Some !ref
        | _ -> failwith "unsupported argument kind"
      in

      match default with
      | None ->
          (arg, kind, desc)
      | Some default ->
          (arg, kind, desc ^ " (default: " ^ default ^ ")")
    ) !specs
  in

  let offset = List.length args in
  let argv = Array.make (Array.length Sys.argv + offset) "" in

  (* First argument is the program name. *)
  argv.(0) <- exe;

  (* Then come the program specified arguments. *)
  ignore (List.fold_left (fun i arg ->
    argv.(i + 1) <- arg;
    i + 1
  ) 0 args);

  (* Finally, the rest of the command line arguments. *)
  for i = offset + 1 to Array.length argv - 1 do
    argv.(i) <- Sys.argv.(i - offset)
  done;

  let inputs = ref [] in

  begin try
    Arg.(parse_argv argv (align specs)
      (fun input -> inputs := input :: !inputs)
      ("Usage: " ^ exe ^ " [option...] <input...>")
    )
  with
  | Arg.Bad msg ->
      Printf.eprintf "%s" msg;
      exit 2
  | Arg.Help msg ->
      Printf.printf "%s" msg;
      exit 0
  end;

  let inputs = List.rev !inputs in

  List.iter (fun f -> f inputs) !actions;

  f inputs
