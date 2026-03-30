let local_start = ref 0.0
let start_cpu = ref (Sys.time ())
let start_real = ref (Unix.gettimeofday ())
let end_cpu = ref 0.0
let end_real = ref 0.0

let start : string -> unit =
 fun s ->
  local_start := Sys.time ();
  print_string s;
  flush stdout
;;

let finish : string -> unit =
 fun _s ->
  print_string ("took " ^ string_of_float (Sys.time () -. !local_start) ^ "s");
  print_endline "";
  local_start := 0.0
;;

let print_log : string -> unit = fun s -> print_endline s

let run : string -> 'a -> 'a =
 fun log f ->
  print_endline log;
  f
;;
