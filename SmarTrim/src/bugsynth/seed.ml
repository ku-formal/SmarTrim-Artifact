(* meta info of a seed file *)
type t = {
  path    : string;
  cmd     : string;
  time    : float; (* execution time *)
  safe    : bool; (* bug-free or not *)
  err     : bool;
  timeout : bool;
}

let mk_init_seed path =
  { path = path;
    cmd = "";
    time = 0.0;
    safe = false;
    err = false;
    timeout = false;
  }

let get_path seed = seed.path
let get_cmd seed = seed.cmd
let get_time seed = seed.time

let is_safe seed = seed.safe
let is_err seed = seed.err
let is_timeout seed = seed.timeout
