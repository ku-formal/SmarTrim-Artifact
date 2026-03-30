open Frontend
open Frontend.Lang
open Vocab
open Vocab.Batteries

type t = {
  pgm : Pgm.t;
  pgm0 : Pgm.t;  (** contains all funcs before removing unreachable *)
  flines : string list;
  cnames : string list;
  seed : int array;  (** used for random number generation *)
  global_vars : Var.t list;
  hardcoded_addrs : Z.Set.t;
  mem : ItvDom.Mem.t;
  struct_map : StructMap.t;
  enums : Enums.t list;
  fmap : FuncMap.t;
  f_defuse : FuncDefUse.t;
  lhs : Node.t Set.t;  (** all loop headers *)
  lhs2 : Node.t Set.t;  (** loop headers within non-pure/non-view functions *)
  lhs3 : Node.t Set.t;
      (** loop headers within functions that are non-pure/non-view and do not contain public
          intcalls *)
  callnodes : (Node.t * Fkey.t) Set.t;
  lhs_main : Node.t Set.t;
  lhs2_main : Node.t Set.t;
  lhs3_main : Node.t Set.t;
  callnodes_main : (Node.t * Fkey.t) Set.t;
  extcalls : Node.t Set.t;
  extcall_locations : Extcall.t Map.Int.t;
  (* ignore effects of external calls *)
  (* verification results are still sound - not for verification purpose *)
  call_edges : CallGraph.edge Set.t;
  base_names : string list;
  base_names_wo_interface : string list;
  struct_handler : Struct_handler.t;
}

module type S = sig
  type t

  val get_basenames : Pgm.t -> string list * string list
  val get_all_fields : t -> Var.t list
  val get_callee_of_intcall : t -> Func.t -> Stmt.t -> Func.t
  val contain_extern : t -> Func.t -> bool
  val is_constant_address : t -> Var.t -> bool
  val find_func_containing_line : int -> t -> Func.t
  val contain_dead_intcall : t -> Func.t -> bool
  val contain_dead_intcall_cfg : t -> Func.t -> bool
  val make_global_info : Pgm.t -> string list -> t
end
