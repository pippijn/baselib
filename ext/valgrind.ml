module Callgrind = struct
  external dump_stats : unit -> unit = "ml_Valgrind_Callgrind_dump_stats"
  external dump_stats_at : string -> unit = "ml_Valgrind_Callgrind_dump_stats_at"
  external zero_stats : unit -> unit = "ml_Valgrind_Callgrind_zero_stats"
  external toggle_collect : unit -> unit = "ml_Valgrind_Callgrind_toggle_collect"
  external start_instrumentation : unit -> unit = "ml_Valgrind_Callgrind_start_instrumentation"
  external stop_instrumentation : unit -> unit = "ml_Valgrind_Callgrind_stop_instrumentation"

  let instrumentation_level = ref 0

  let instrumented f x =
    if !instrumentation_level = 0 then
      start_instrumentation ();

    incr instrumentation_level;

    let r, e =
      try
        Some (f x), None
      with e ->
        None, Some e
    in

    decr instrumentation_level;

    if !instrumentation_level = 0 then
      stop_instrumentation ();

    match r, e with
    | Some r, None -> r
    | None, Some e -> raise e
    | _ -> failwith "impossible"
end

module Memcheck = struct
  type leak_info = {
    leaked : int;
    dubious : int;
    reachable : int;
    suppressed : int;
  }

  external make_mem_noaccess : int -> int -> unit = "ml_Valgrind_Memcheck_make_mem_noaccess"
  external make_mem_undefined : int -> int -> unit = "ml_Valgrind_Memcheck_make_mem_undefined"
  external make_mem_defined : int -> int -> unit = "ml_Valgrind_Memcheck_make_mem_defined"
  external make_mem_defined_if_addressable : int -> int -> unit = "ml_Valgrind_Memcheck_make_mem_defined_if_addressable"
  external create_block : int -> int -> string -> unit = "ml_Valgrind_Memcheck_create_block"
  external discard : int -> unit = "ml_Valgrind_Memcheck_discard"
  external check_mem_is_addressable : int -> int -> unit = "ml_Valgrind_Memcheck_check_mem_is_addressable"
  external check_mem_is_defined : int -> int -> unit = "ml_Valgrind_Memcheck_check_mem_is_defined"
  external do_leak_check : unit -> unit = "ml_Valgrind_Memcheck_do_leak_check"
  external do_added_leak_check : unit -> unit = "ml_Valgrind_Memcheck_do_added_leak_check"
  external do_changed_leak_check : unit -> unit = "ml_Valgrind_Memcheck_do_changed_leak_check"
  external do_quick_leak_check : unit -> unit = "ml_Valgrind_Memcheck_do_quick_leak_check"
  external count_leaks : unit -> leak_info = "ml_Valgrind_Memcheck_count_leaks"
  external count_leak_blocks : unit -> leak_info = "ml_Valgrind_Memcheck_count_leak_blocks"
  external get_vbits : int -> int -> int -> int = "ml_Valgrind_Memcheck_get_vbits"
  external set_vbits : int -> int -> int -> int = "ml_Valgrind_Memcheck_set_vbits"
end

external sizeof : 'a -> int = "ml_Valgrind_sizeof"
