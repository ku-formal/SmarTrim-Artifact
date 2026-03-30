(* meta info of a mutant file *)
type t = {
  path    : string;
  cmd_v   : string;
  cmd_e   : string;
  time_v  : float;
  time_e  : float;
  buggy   : bool;
  safe    : bool;
  pending : bool;
  err_v   : bool;
  err_e   : bool;
  timeout_v : bool;
  timeout_e : bool;
}

let mk_init_mutant path =
  { path = path;
    cmd_v   = "";
    cmd_e   = "";
    time_v  = 0.0;
    time_e  = 0.0;
    buggy   = false;
    safe    = false;
    pending = false;
    err_v   = false;
    err_e   = false;
    timeout_v = false;
    timeout_e = false;
  }

let get_path mutant = mutant.path
let get_cmd_v mutant = mutant.cmd_v
let get_cmd_e mutant = mutant.cmd_e
let get_time_v mutant = mutant.time_v
let get_time_e mutant = mutant.time_e

let is_buggy mutant = mutant.buggy
let is_safe mutant = mutant.safe
let is_pending mutant = mutant.pending

let is_err_v mutant = mutant.err_v
let is_err_e mutant = mutant.err_e
let is_err mutant = is_err_v mutant || is_err_e mutant

let is_timeout_v mutant = mutant.timeout_v
let is_timeout_e mutant = mutant.timeout_e
let is_timeout mutant = is_timeout_v mutant || is_timeout_e mutant
