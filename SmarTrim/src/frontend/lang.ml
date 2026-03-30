open Options
open Vocab.Pp

module Node = struct
  type t = ENTRY | EXIT | Node of int [@@deriving compare, equal, show { with_path = false }]

  let hash : t -> int = Hashtbl.hash
  let entry : t = ENTRY
  let exit : t = EXIT
  let is_entry n = equal n ENTRY
  let is_exit n = equal n EXIT
  let trans = Node 0
  let extern = Node (-1)

  open struct
    let nid = ref 0
  end

  let make () =
    incr nid;
    Node !nid
  ;;

  let to_string (n : t) : string =
    match n with ENTRY -> "ENTRY" | EXIT -> "EXIT" | Node i -> string_of_int i
  ;;
end

module G = Graph.Persistent.Digraph.Concrete (Node)

(********************)
(********************)
(***** language *****)
(********************)
(********************)

(** Solidity Enum. *)
module Enums = struct
  type t = { id : string; members : string list } [@@deriving show { with_path = false }]
end

(** Location. *)
module Loc = struct
  open Ppx_compare_lib.Builtin

  type t = { line : int; finish_line : int; offset : int; (* in byte *) len : int (* in byte *) }
  [@@deriving show { with_path = false }, compare]

  let dummy : t = { line = -1; finish_line = -1; offset = -1; len = -1 }

  let mk ?(line = -1) ?(finish_line = -1) ?(offset = -1) ?(len = -1) () =
    let finish_line = max line finish_line in
    { line; finish_line; offset; len }
  ;;

  let pp ?(machine = false) ppf ({ line; finish_line; offset; len } as l) : unit =
    if machine then pp ppf l else pf ppf "%i:%i:%i:%i" line finish_line offset len
  ;;

  let show ?(machine = false) (loc : t) = str (pp ~machine) loc
end

(** Unary Operators. *)
module Uop = struct
  type t = Pos | Neg | LNot | BNot [@@deriving show { with_path = false }]

  let pp ?(machine = false) ppf o =
    if machine then pp ppf o
    else
      match o with Pos -> pf ppf "+" | Neg -> pf ppf "-" | LNot -> pf ppf "!" | BNot -> pf ppf "~"
  ;;

  let show ?(machine = false) o = str (pp ~machine) o
end

(** Binary Operators. *)
module Bop = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Exponent
    | GEq
    | Gt
    | LEq
    | Lt
    | LAnd
    | LOr
    | Eq
    | NEq
    | ShiftL
    | ShiftR
    | BXor
    | BAnd
    | BOr
  [@@deriving show { with_path = false }]

  let show ?(machine = false) b =
    if machine then show b
    else
      match b with
      | Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
      | Mod -> "%"
      | Exponent -> "**"
      | GEq -> ">="
      | Gt -> ">"
      | LEq -> "<="
      | Lt -> "<"
      | LAnd -> "&&"
      | LOr -> "||"
      | Eq -> "=="
      | NEq -> "!="
      | ShiftL -> "<<"
      | ShiftR -> ">>"
      | BXor -> "^"
      | BAnd -> "&"
      | BOr -> "|"
  ;;

  let pp ?(machine = false) ppf b = if machine then pp ppf b else Pp.pf ppf "%s" (show ~machine b)
end

module Ckind = struct
  type t = Contract | Interface | Library [@@deriving show { with_path = false }]

  let of_string str =
    match str with
    | "contract" -> Contract
    | "interface" -> Interface
    | "library" -> Library
    | _ -> invalid_arg "VeriSmart.Frontend.Lang.Ckind.of_string"
  ;;
end

module Directive = struct
  type t =
    | Typ of Typ.t  (** solidity syntax [using LIB for TYP] *)
    | Glob  (** solidity syntax [using LIB for *] *)
  [@@deriving show { with_path = false }]
end

module Cinfo = struct
  type t = {
    numid : int;
    inherit_order : int list;
    lib_typ_list : (string * Directive.t) list;
        (** using directive [using LIB for TYP]. a list of pairs of (lib name, aliased type). Orders
            do not matter. *)
    ckind : Ckind.t;
  }
  [@@deriving show { with_path = false }]
end

module Einfo = struct
  type t = { typ : Typ.t; loc : Loc.t; id : int } [@@deriving show { with_path = false }]
end

module Ifinfo = struct
  type t = {
    if_loc : Loc.t;
    if_tloc : Loc.t;
    if_floc : Loc.t option;  (** [None] means no 'else' block exists in original code *)
  }
  [@@deriving show { with_path = false }]

  let dummy = { if_loc = Loc.dummy; if_tloc = Loc.dummy; if_floc = Some Loc.dummy }
end

(** Visibility. *)
module Vis = struct
  type t = Public | Internal | External | Private [@@deriving show { with_path = false }]

  let pp ?(machine = false) ppf vis =
    if machine then pp ppf vis
    else
      let ctt =
        match vis with
        | Public -> "public"
        | Internal -> "internal"
        | External -> "external"
        | Private -> "private"
      in
      pf ppf "%s" ctt
  ;;

  let show ?(machine = false) vis = if machine then show vis else str (pp ~machine) vis
end

module State_mut = struct
  type t = Payable | NonPayable | View | Pure [@@deriving show { with_path = false }]
end

(** function key *)
module Fkey = struct
  type t = {
    contract : string;  (** contract name *)
    func : string;  (** function name *)
    param_typs : Typ.t list;  (** parameter types *)
  }
  [@@deriving show { with_path = false }]

  let to_string { contract; func; param_typs } : string =
    "(" ^ contract ^ "::" ^ func ^ ", "
    ^ string_of_list ~first:"[" ~last:"]" ~sep:", " Typ.to_string param_typs
    ^ ")"
  ;;

  let mk contract func param_typs : t = { contract; func; param_typs }
  let empty = mk "" "" []
  let address_fallback = mk "@address" "@fallback" []
  let address_receive = mk "@address" "@receive" []
end

module Fsig = struct
  type t = string * Typ.t list
end

type exp =
  | V of Value.t
  | Str of string
  | Lv of lv
  | Cast of Typ.t * exp
  | BinOp of Bop.t * exp * exp * Einfo.t
  | UnOp of Uop.t * exp * Typ.t
  | Ite of exp * exp * exp * Typ.t  (** if-then-else ternary operator *)
  | ETypeName of Typ.e  (** may be second arguments of abi.decode functions *)
  | IndexRangeAccess of lv * exp option * exp option * Einfo.t (* base, start op, end op *)
  | TypeInfo of Typ.t * string * Einfo.t (* type(T).fieldname *)
  (* exists only in the interim process *)
  | IncTemp of exp * bool * Loc.t (* true if prefix, false if postfix *)
  | DecTemp of exp * bool * Loc.t
  | CallTemp of
      exp
      * exp list
      * exp option
      * exp option
      * (* ether, gas *)
        Einfo.t
  | CondTemp of exp * exp * exp * Typ.t * Loc.t
  | AssignTemp of lv * exp * Loc.t

and lv =
  | Var of (string * vinfo)
  | MemberAccess of exp * string * vinfo * Typ.t (* exp.id *)
  | IndexAccess of exp * exp option * Typ.t (* exp[exp?] XXX: not sure why 'option' exists *)
  | Tuple of exp option list * Typ.t

and vinfo = {
  vloc : Loc.t;
  is_gvar : bool;
  vtyp : Typ.t;
  vvis : Vis.t;
  vid : int;
  refid : int;  (** referenced declartion. valid only for non-function variables *)
  vscope : int;  (** belonging contract numid (global) or func numid (local) *)
  storage : string;
  flag : bool;  (** true if the information is propagated *)
  org : exp option;  (** original expression (source code) before renamed or replaced *)
}

let get_type_var (var : Var.t) : Typ.t = snd var
let get_type_var2 : string * vinfo -> Typ.t = fun (_v, vinfo) -> vinfo.vtyp

let get_type_lv lv : Typ.t =
  match lv with
  | Var (_, vinfo) -> vinfo.vtyp
  | MemberAccess (_, _, _, typ) -> typ
  | IndexAccess (_, _, typ) -> typ
  | Tuple (_, typ) -> typ
;;

let get_type_exp exp : Typ.t =
  match exp with
  | V v -> Value.typ v
  | Str s -> ConstString { value = s }
  | Lv lv -> get_type_lv lv
  | Cast (typ, _) -> typ
  | BinOp (_, _, _, einfo) -> einfo.typ
  | UnOp (_, _, typ) -> typ
  | ETypeName etyp -> EType etyp
  | IndexRangeAccess (_, _, _, einfo) -> einfo.typ
  | TypeInfo (_, _, einfo) -> einfo.typ
  | _ -> failwith "get_type_exp"
;;

module Extcall = struct
  type t = {
    id : int;
    ret : lv option;
    fkey : Fkey.t;
    target : exp;
    args : exp list;
    ether : exp option;
    gas : exp option;
    is_static : bool;
    loc : Loc.t;
  }
  [@@deriving fields ~getters]

  let compare l r = Int.compare l.id r.id
  let is_toward_address e = Typ.is_address_kind (get_type_exp e.target)
  let no_return e = Option.is_none e.ret
end

module Stmt = struct
  type t =
    | Assign of lv * exp * Loc.t
    | Decl of lv
    | Seq of t * t
    | Call of lv option * exp * exp list * exp option * exp option * Loc.t
        (** return assignment, callee, args, ether, gas, loc *)
    | Extcall of Extcall.t
    | Skip
    | If of exp * t * t * Ifinfo.t
    | While of exp * t
    | Break
    | Continue
    | Return of exp option * Loc.t
    | Revert
    | Assume of exp * Loc.t
    | Assert of exp * string * Loc.t  (** string : vulnerability type *)
    | Assembly of (string * int) list * Loc.t
    | Placeholder
    | Unchecked of t list * Loc.t  (** unchecked block *)
    | Label of string  (** use this for any experiment feature *)

  let is_skip s = match s with Skip -> true | _ -> false
  let list_to_seq l = if List.is_empty l then Skip else List.reduce (fun s1 s2 -> Seq (s1, s2)) l
end

module Mod_call = struct
  type t = { id : string; args : exp list; loc : Loc.t }

  let id m = m.id
  let args m = m.args
  let loc m = m.loc
end

module Cfg = struct
  type t = {
    graph : G.t;
    outpreds_of_lh : Node.t Set.t;  (** preds of loop headers outside of loops *)
    lh_set : Node.t Set.t;  (** loop header set *)
    lx_set : Node.t Set.t;  (** loop exit set *)
    continue_set : Node.t Set.t;
    break_set : Node.t Set.t;
    basic_paths : Node.t list Set.t;
    stmt_map : (Node.t, Stmt.t) Map.t;
    signature : Fkey.t;
  }

  let empty =
    let e = Set.empty in
    {
      graph = G.empty;
      outpreds_of_lh = e;
      lh_set = e;
      lx_set = e;
      continue_set = e;
      break_set = e;
      basic_paths = e;
      stmt_map = Map.empty;
      signature = Fkey.{ contract = "Dummy"; func = "Dummy"; param_typs = [] };
    }
  ;;

  let find_stmt n g =
    try if n = Node.ENTRY || n = Node.EXIT then Stmt.Skip else Map.find n g.stmt_map
    with Not_found -> failwith ("No stmt found in the given node " ^ Node.to_string n)
  ;;

  let nodes_of g = G.fold_vertex (fun x acc -> x :: acc) g.graph []

  (** inspect whether loop headers exist *)
  let has_loop g = not (Set.is_empty g.lh_set)

  let is_outer_pred_of_lh n g = Set.mem n g.outpreds_of_lh
  let is_loophead n g = Set.mem n g.lh_set
  let is_loopexit n g = Set.mem n g.lx_set
  let is_continue_node n g = Set.mem n g.continue_set
  let is_break_node n g = Set.mem n g.break_set
  let is_callnode n g = match find_stmt n g with Call _ | Extcall _ -> true | _ -> false
  let is_skip_node n g = match find_stmt n g with Skip -> true | _ -> false

  let is_exception_node n g =
    match find_stmt n g with
    | Revert -> true
    | Call (_, Lv (Var ("revert", _)), _, _, _, _) -> true
    | _ -> false
  ;;

  let is_assign_node n g = match find_stmt n g with Assign _ -> true | _ -> false
end

module Finfo = struct
  type t = {
    is_constructor : bool;
    is_payable : bool;
    is_modifier : bool;
    mod_list : Mod_call.t list;
    mod_list2 : Mod_call.t list;  (** constructor modifier invocations *)
    param_loc : Loc.t;  (** line of '(' and ')' *)
    ret_param_loc : Loc.t;  (** location where ret params are declared *)
    fvis : Vis.t;
    mutability : State_mut.t;
    fid : int;
    floc : Loc.t;  (** start line: 'function' keyword, endling line: '\}' *)
    blk_loc : Loc.t;
    scope : int;  (** belonging contract numid *)
    scope_s : string;  (** belonging contract name *)
    org_scope_s : string;  (** original contract name in which functions are initially defined *)
    cfg : Cfg.t;
  }
end

module Var2 = struct
  type t = { id : string; info : vinfo } [@@deriving fields ~getters ~fields]

  let mk id info = { id; info }
  let to_var v : Var.t = (v.id, v.info.vtyp)
  let to_string v = Typ.to_string v.info.vtyp ^ " " ^ v.id
end

module Func = struct
  type t = {
    name : string;
    params : (string * vinfo) list;
    ret_params : (string * vinfo) list;
    body : Stmt.t;
    extcall_locations : Extcall.t list;
        (** this field is initially an empty list, and filled during {!Preprocess2.run} step. *)
    info : Finfo.t;
  }
  [@@deriving fields ~fields ~getters]

  let mk ~name ~params ~ret_params ~body ~info : t =
    { name; params; ret_params; body; extcall_locations = []; info }
  ;;

  let vis f = f.info.fvis
  let mod_calls f = f.info.mod_list
  let mutability f = f.info.mutability
  let body f = f.body
  let blk_loc f = f.info.blk_loc
  let cfg f = f.info.cfg
  let param_vars f = List.map (fun (v, vinfo) -> (v, vinfo.vtyp)) f.params
  let param_types f : Typ.t list = List.map (fun p -> (snd p).vtyp) f.params
  let ret_param_vars f = List.map (fun (v, vinfo) -> (v, vinfo.vtyp)) f.ret_params
  let ret_param_types f = List.map (fun p -> (snd p).vtyp) f.ret_params
  let fsig f : Fsig.t = (f.name, param_types f)
  let fkey f = Fkey.{ contract = f.info.scope_s; func = f.name; param_typs = param_types f }
  let is_public f = vis f = Public
  let is_external f = vis f = External
  let is_internal f = vis f = Internal
  let is_private f = vis f = Private

  let is_view_pure f =
    let mut = mutability f in
    mut = View || mut = Pure
  ;;

  let is_payable f = f.info.is_payable
  let is_constructor f = f.info.is_constructor
  let is_modifier f = f.info.is_modifier
  let equal_sig f1 f2 = fsig f1 = fsig f2
end

module State_var_decl = struct
  type t = {
    name : string;  (** name. also used as an id. *)
    assign : exp option;  (** todo: remove option (we can simply assign a default value) *)
    vinfo : vinfo;
  }

  let equal d1 d2 = String.equal d1.name d2.name
  let to_var t = Var.mk t.name t.vinfo.vtyp
end

module Struct = struct
  type t = { name : string; fields : Var2.t list }
end

module Contract = struct
  type t = {
    name : string;  (** contract class name *)
    decls : State_var_decl.t list;  (** global var declarations *)
    structs : Struct.t list;
    enums : Enums.t list;
    funcs : Func.t list;
    info : Cinfo.t;
  }
  [@@deriving fields ~getters ~fields]

  let numid c = c.info.numid
  let inherit_order c = c.info.inherit_order
  let fnames c = c.funcs |> List.map (fun f -> f.Func.name)
  let is_library c = c.info.ckind = Library
  let is_interface c = c.info.ckind = Interface
  let is_contract c = c.info.ckind = Contract

  let cnstr (c : t) : Func.t =
    let lst = List.filter (fun (f : Func.t) -> f.info.is_constructor) c.funcs in
    let _ = assert (List.length lst = 1) in
    List.hd lst
  ;;

  let gvars c : Var.t list = List.map State_var_decl.(fun d -> (d.name, d.vinfo.vtyp)) c.decls

  let accessible_gvars c : Var.t list =
    let open State_var_decl in
    c.decls
    |> List.filter (fun d -> d.vinfo.vscope = c.info.numid || d.vinfo.vvis <> Private)
    |> List.map (fun d -> (d.name, d.vinfo.vtyp))
  ;;

  let find_fn_by_loc (loc : Loc.t) (c : t) : Func.t option =
    List.find_opt
      (fun (f : Func.t) ->
        let floc = f.info.floc in
        floc.offset <= loc.offset && loc.offset < floc.offset + floc.len)
      c.funcs
  ;;
end

(** A program is a list of contracts. *)
module Pgm = struct
  type t = Contract.t list

  let find_by_name (pgm : t) (name : string) = List.find (fun (c : Contract.t) -> c.name = name) pgm
  let find_nid (pgm : t) (nid : int) = List.find (fun (c : Contract.t) -> nid = c.info.numid) pgm

  (** Return the main contract. *)
  let main (pgm : t) : Contract.t =
    if String.equal !main_contract "" then List.last pgm
    else
      try find_by_name pgm !main_contract
      with _ -> failwith ("main contract name mathcing failed : " ^ "\'" ^ !main_contract ^ "\'")
  ;;

  let structs (pgm : t) : Struct.t list =
    List.fold_left (fun acc (c : Contract.t) -> c.structs @ acc) [] pgm
  ;;

  let cnames (pgm : t) : string list = List.map (fun (c : Contract.t) -> c.name) pgm

  let gvars (p : t) : Var.t list =
    let open State_var_decl in
    let main = main p in
    let decls = main.decls in
    List.map (fun d -> (d.name, d.vinfo.vtyp)) decls
  ;;

  let libnames (pgm : t) : string list =
    let open Contract in
    let libs = List.filter (fun c -> c.info.ckind = Library) pgm in
    List.map (fun c -> c.name) libs
  ;;

  let modify (c : Contract.t) (p : t) : t =
    let cname = c.name in
    List.map (fun (c' : Contract.t) -> if String.equal cname c'.name then c else c') p
  ;;

  let find_fn_by_loc (loc : Loc.t) (p : t) : Func.t option =
    let rec aux loc contracts =
      match contracts with
      | [] -> None
      | h :: t -> begin
        match Contract.find_fn_by_loc loc h with Some fn -> Some fn | None -> aux loc t
      end
    in
    aux loc p
  ;;
end

let dummy_vinfo =
  {
    vloc = Loc.dummy;
    is_gvar = false;
    vtyp = Void;
    vvis = Private;
    vid = -1;
    refid = -1;
    vscope = 1;
    storage = "";
    flag = false;
    org = None;
  }
;;

let mk_vinfo ?(loc = Loc.dummy) ?(typ = Typ.Void) ?(org = None) () =
  {
    vloc = loc;
    is_gvar = false;
    vtyp = typ;
    vvis = Private;
    vid = -1;
    refid = -1;
    vscope = -1;
    storage = "";
    flag = false;
    org;
  }
;;

(** @raise Invalid_arg if [stmt] is not [Extcall] *)
let get_external_call_obj_exn (stmt : Stmt.t) : exp =
  match stmt with Extcall { target; _ } -> target | _ -> invalid_arg ""
;;

(** @raise Invalid_arg if [exp] is not [Lv] *)
let exp_to_lv exp = match exp with Lv lv -> lv | _ -> invalid_arg "exp_to_lv"

(** If an expression [e] is [<address>.<member>], return [(address, member)]. *)
let get_target_and_mem e =
  match e with
  | Lv (MemberAccess (rcv, mem, _, _)) when Typ.is_contract (get_type_exp rcv) -> Some (rcv, mem)
  | _ -> None
;;

let get_bigint (exp : exp) : Z.t =
  match exp with V (Int bigint) -> bigint | _ -> failwith "get_bigint"
;;

(* 0 <= X < 2^n *)
let rec bit_unsigned_of_int (n : Z.t) bit : int =
  let open Z in
  assert (bit <= 256);
  if lt n (pow (of_int 2) bit) then bit (* meaning EType (UInt bit) *)
  else bit_unsigned_of_int n (Int.add bit 8)
;;

(* -2^(n-1) <= X < 2^(n-1) *)
let rec bit_signed_of_int (n : Z.t) bit : int =
  let open Z in
  assert (bit <= 256);
  let bit' = Int.sub bit 1 in
  if geq n (neg (pow (of_int 2) bit')) && lt n (pow (of_int 2) bit') then bit
    (* meaning EType (SInt bit) *)
  else bit_signed_of_int n (Int.add bit 8)
;;

let get_all_fkeys_c (c : Contract.t) : Fkey.t Set.t = Set.of_list (List.map Func.fkey c.funcs)

let get_all_fkeys (p : Pgm.t) : Fkey.t Set.t =
  List.fold_left (fun acc c -> Set.union (get_all_fkeys_c c) acc) Set.empty p
;;

(******************************)
(******************************)
(***** Tostring Functions *****)
(******************************)
(******************************)

let rec to_string_exp ?(solv = Solc.Ver.mk 0 4 25) ?(report = false) exp =
  match exp with
  | V v -> Value.to_string v
  | Str s ->
    if !cfg then "\\\"" ^ String.nreplace ~str:s ~sub:"\n" ~by:"" ^ "\\\""
    else "\"" ^ String.nreplace ~str:s ~sub:"\n" ~by:"" ^ "\""
  | Lv lv -> to_string_lv ~solv ~report lv
  | Cast (typ, e) -> Typ.to_string typ ^ "(" ^ to_string_exp ~solv ~report e ^ ")"
  | BinOp (bop, e1, e2, _) ->
    "(" ^ to_string_exp ~solv ~report e1 ^ " " ^ Bop.show bop ^ " " ^ to_string_exp ~solv ~report e2
    ^ ")"
  | UnOp (uop, e, _) -> "(" ^ Uop.show uop ^ to_string_exp ~solv ~report e ^ ")"
  | Ite (i, t, e, _) ->
    Pp.spf "Ite(%s, %s, %s)" (to_string_exp ~solv ~report i) (to_string_exp ~solv ~report t)
      (to_string_exp ~solv ~report e)
  | ETypeName etyp -> Typ.to_string_e etyp
  | IndexRangeAccess (base, sop, fop, _) -> (
    match (sop, fop) with
    | Some s, Some f ->
      to_string_lv ~solv ~report base ^ "[" ^ to_string_exp ~solv ~report s ^ ":"
      ^ to_string_exp ~solv ~report f ^ "]"
    | Some s, None ->
      to_string_lv ~solv ~report base ^ "[" ^ to_string_exp ~solv ~report s ^ ":" ^ "]"
    | None, Some f ->
      to_string_lv ~solv ~report base ^ "[" ^ ":" ^ to_string_exp ~solv ~report f ^ "]"
    | None, None -> assert false)
  | TypeInfo (typ, x, _) -> "type(" ^ Typ.to_string typ ^ ")." ^ x
  | IncTemp (e, prefix, _) -> if prefix then "++" ^ to_string_exp e else to_string_exp e ^ "++"
  | DecTemp (e, prefix, _) -> if prefix then "--" ^ to_string_exp e else to_string_exp e ^ "--"
  | CondTemp (e1, e2, e3, _, _) ->
    "(" ^ to_string_exp e1 ^ " ? " ^ to_string_exp e2 ^ " : " ^ to_string_exp e3 ^ ")"
  | AssignTemp (lv, e, _) -> "(" ^ to_string_lv lv ^ " = " ^ to_string_exp e ^ ")"
  | CallTemp (e, args, ethop, gasop, _) ->
    to_string_exp ~solv ~report e
    ^ to_string_ethop_gasop ~solv ~report ethop gasop
    ^ string_of_list ~first:"(" ~last:")" ~sep:", " (to_string_exp ~solv ~report) args

and to_string_exp_opt ?(solv = Solc.Ver.mk 0 4 25) ?(report = false) exp =
  match exp with Some e -> to_string_exp ~solv ~report e | None -> " "

and to_string_ethop_gasop ?(solv = Solc.Ver.mk 0 4 25) ?(report = false) ethop gasop =
  if solv < Solc.Ver.mk 0 7 0 then
    (match ethop with None -> "" | Some e -> ".value(" ^ to_string_exp ~solv ~report e ^ ")")
    ^ match gasop with None -> "" | Some e -> ".gas(" ^ to_string_exp ~solv ~report e ^ ")"
  else
    match (ethop, gasop) with
    | None, None -> ""
    | Some eth, None -> "{value: " ^ to_string_exp ~solv ~report eth ^ "}"
    | None, Some gas -> "{gas: " ^ to_string_exp ~solv ~report gas ^ "}"
    | Some eth, Some gas ->
      "{value: " ^ to_string_exp ~solv ~report eth ^ ", " ^ "gas: "
      ^ to_string_exp ~solv ~report gas ^ "}"

and to_string_lv ?(solv = Solc.Ver.mk 0 4 25) ?(report = false) lv =
  match lv with
  | Var (x, xinfo) -> if not report then x else to_string_vinfo_org ~report x xinfo.org
  | MemberAccess (e, x, xinfo, _) ->
    to_string_exp ~solv ~report e ^ "."
    ^ if not report then x else to_string_vinfo_org ~report x xinfo.org
  | IndexAccess (e, None, _) -> to_string_exp ~solv ~report e ^ "[]"
  | IndexAccess (e1, Some e2, _) ->
    to_string_exp ~solv ~report e1 ^ "[" ^ to_string_exp ~solv ~report e2 ^ "]"
  | Tuple (elst, t) ->
    if Typ.is_array t then
      string_of_list ~first:"[" ~last:"]" ~sep:", " (to_string_exp_opt ~report) elst
    else string_of_list ~first:"(" ~last:")" ~sep:", " (to_string_exp_opt ~report) elst

and to_string_vinfo_org ?(solv = Solc.Ver.mk 0 4 25) ?(report = false) x org =
  match org with None -> x | Some e -> to_string_exp ~solv ~report e
;;

let rec to_string_stmt ?(solv = Solc.Ver.mk 0 4 25) ?(report = false) (stmt : Stmt.t) =
  match stmt with
  | Assign (lv, e, _) ->
    to_string_lv ~solv ~report lv ^ " := " ^ to_string_exp ~solv ~report e ^ ";"
  | Decl lv -> Typ.to_string (get_type_lv lv) ^ " " ^ to_string_lv lv ^ ";"
  | Seq (s1, s2) ->
    to_string_stmt ~solv ~report s1 ^ "" ^ "\n" ^ "    " ^ to_string_stmt ~solv ~report s2
  | Call (None, e, exps, ethop, gasop, _) ->
    to_string_exp ~solv ~report e
    ^ (match ethop with None -> "" | Some e -> ".value(" ^ to_string_exp ~solv ~report e ^ ")")
    ^ (match gasop with None -> "" | Some e -> ".gas(" ^ to_string_exp ~solv ~report e ^ ")")
    ^ string_of_list ~first:"(" ~last:")" ~sep:", " (to_string_exp ~solv ~report) exps
    ^ ";"
  | Call (Some lv, e, exps, ethop, gasop, _) ->
    (* NOTE: refer to 'replace_tmpexp_e' in preprocess.ml *)
    if report && String.starts_with (to_string_lv lv) "@Tmp" then to_string_lv ~solv ~report lv
    else
      to_string_lv ~solv ~report lv ^ " = " ^ to_string_exp ~solv ~report e
      ^ to_string_ethop_gasop ~solv ~report ethop gasop
      ^ string_of_list ~first:"(" ~last:")" ~sep:", " (to_string_exp ~solv ~report) exps
      ^ ";"
  | Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc } ->
    ignore (ether, gas, loc);
    let static = if is_static then "<static>" else "" in
    Pp.spf {|%s@extern%s#%i[%s.(%s::%s)(%s)];|}
      (if Option.is_some ret then Pp.spf "%s = " (to_string_lv (Option.get ret)) else "")
      static id
      (to_string_exp ~solv ~report target)
      fkey.contract fkey.func
      (string_of_list ~first:"" ~last:"" ~sep:", " (to_string_exp ~solv ~report) args)
  | Skip -> "skip;"
  | If (e, s1, s2, _) ->
    "if" ^ "(" ^ to_string_exp ~solv ~report e ^ ")" ^ "{" ^ to_string_stmt ~solv ~report s1 ^ "}"
    ^ " " ^ "else {" ^ to_string_stmt ~solv ~report s2 ^ "} if_end;"
  | While (e, s) ->
    "while" ^ "(" ^ to_string_exp ~solv ~report e ^ ")" ^ "{" ^ to_string_stmt ~solv ~report s
    ^ "} while_end;"
  | Break -> "break;"
  | Continue -> "continue;"
  | Return (None, _) -> "return;"
  | Return (Some e, _) -> "return " ^ to_string_exp ~solv ~report e ^ ";"
  | Revert -> "revert;"
  | Assume (e, _) -> "assume(" ^ to_string_exp ~solv ~report e ^ ");"
  | Assert (e, _, _) -> "assert(" ^ to_string_exp ~solv ~report e ^ ");"
  | Assembly (lst, _) ->
    "assembly" ^ string_of_list ~first:"{" ~last:"}" ~sep:", " (fst |> id) lst ^ ";"
  | Placeholder -> "_;"
  | Unchecked (lst, _) ->
    "unchecked {\n"
    ^ List.fold_left
        (fun acc s ->
          if acc = "" then "    " ^ to_string_stmt ~solv ~report s
          else acc ^ "\n" ^ "    " ^ to_string_stmt ~solv ~report s)
        "" lst
    ^ "\n" ^ "}"
  | Label _ -> ""
;;

let rec to_string_func Func.{ name; params; ret_params; body; extcall_locations = _; info } =
  "function" ^ " " ^ name ^ " " ^ to_string_params params
  ^ (if List.length info.mod_list2 > 0 then " " ^ to_string_mods info.mod_list2 else "")
  ^ (if List.length info.mod_list > 0 then " " ^ to_string_mods info.mod_list else "")
  ^ " " ^ "returns" ^ " " ^ to_string_params ret_params ^ " " ^ Vis.show info.fvis ^ " "
  ^ (if info.is_payable then "payable" else "")
  ^ " " ^ "{" ^ "\n" ^ "    " ^ to_string_stmt body ^ "\n" ^ "  " ^ "}" ^ "\n"

and to_string_param (id, vinfo) = Typ.to_string vinfo.vtyp ^ " " ^ id
and to_string_params params = string_of_list ~first:"(" ~last:")" ~sep:", " to_string_param params
and to_string_exps exps = string_of_list ~first:"(" ~last:")" ~sep:", " to_string_exp exps

and to_string_mod (m : Mod_call.t) =
  if List.length m.args = 0 then m.id else m.id ^ to_string_exps m.args

and to_string_mods mods = string_of_list ~first:"[" ~last:"]" ~sep:" " to_string_mod mods

let to_string_state_var_decl (d : State_var_decl.t) =
  match d.assign with
  | None -> Typ.to_string d.vinfo.vtyp ^ " " ^ d.name ^ ";"
  | Some e -> Typ.to_string d.vinfo.vtyp ^ " " ^ d.name ^ " = " ^ to_string_exp e ^ ";"
;;

let to_string_structure (s : Struct.t) =
  "struct" ^ " " ^ s.name ^ "{" ^ "\n"
  ^ string_of_list ~first:"    " ~last:";" ~sep:";\n    " Var2.to_string s.fields
  ^ "\n" ^ "  " ^ "}" ^ "\n"
;;

let to_string_enum (e : Enums.t) =
  "enum" ^ " " ^ e.id ^ string_of_list ~first:" {" ~last:"}" ~sep:", " id e.members
;;

let to_string_contract ({ name; decls; structs; enums; funcs; _ } : Contract.t) =
  "contract" ^ " " ^ name ^ " {\n"
  ^ (if decls = [] then ""
     else string_of_list ~first:"  " ~last:"\n\n" ~sep:"\n  " to_string_state_var_decl decls)
  ^ (if structs = [] then ""
     else string_of_list ~first:"  " ~last:"\n\n" ~sep:"\n  " to_string_structure structs)
  ^ (if enums = [] then ""
     else string_of_list ~first:"  " ~last:"\n\n" ~sep:"\n  " to_string_enum enums)
  ^ string_of_list ~first:"  " ~last:"\n" ~sep:"\n  " to_string_func funcs
  ^ "}"
;;

let to_string_pgm contracts =
  string_of_list ~first:"" ~last:"" ~sep:"\n\n" to_string_contract contracts
;;

let to_string_fsig (fname, typs) =
  fname ^ ", " ^ string_of_list ~first:"{" ~last:"}" ~sep:", " Typ.to_string typs
;;

let to_string_fkey2 Fkey.{ contract; func; param_typs } : string =
  "(" ^ contract ^ "/" ^ func ^ "/"
  ^ string_of_list ~first:"[" ~last:"]" ~sep:"_" Typ.to_string param_typs
  ^ ")"
;;

let to_string_cfg ?(name = "G") (cfg : Cfg.t) : string =
  "digraph " ^ name ^ "{\n" ^ "{\n" ^ "node [shape=box]\n"
  ^ G.fold_vertex
      (fun v acc ->
        let str_v = Node.to_string v in
        let stmt = to_string_stmt (Cfg.find_stmt v cfg) in
        let colored =
          if Cfg.is_loophead v cfg then " style=filled color=grey shape=oval"
          else if Cfg.is_loopexit v cfg then " style=filled color=grey shape=diamond"
          else if Cfg.is_callnode v cfg then " style=filled color=yellow"
          else ""
        in
        acc ^ str_v ^ " [label=\"" ^ str_v ^ ": " ^ stmt ^ "\"" ^ colored ^ "]" ^ "\n")
      cfg.graph ""
  ^ "}" ^ "\n"
  ^ G.fold_edges
      (fun v1 v2 acc -> acc ^ Node.to_string v1 ^ " -> " ^ Node.to_string v2 ^ "\n")
      cfg.graph ""
  ^ "}" ^ "\n\n"
;;

let to_string_cfg_f (func : Func.t) : string = to_string_cfg ~name:func.name (Func.cfg func)

let to_string_cfg_c (contract : Contract.t) : string =
  List.fold_left (fun acc f -> acc ^ to_string_cfg_f f) "" contract.funcs
;;

let to_string_cfg_p (p : Pgm.t) : string =
  List.fold_left (fun acc c -> acc ^ to_string_cfg_c c) "" p
;;

let to_string_path (path : Node.t list) : string =
  string_of_list ~first:"[" ~last:"]" ~sep:"->" Node.to_string path
;;

let to_string_paths (paths : Node.t list Set.t) : string =
  string_of_set ~first:"{" ~last:"}" ~sep:",\n" to_string_path paths
;;

(******************************)
(******************************)
(***** Built-in Functions *****)
(******************************)
(******************************)

let is_require exp = match exp with Lv (Var ("require", _)) -> true | _ -> false
let is_assert exp = match exp with Lv (Var ("assert", _)) -> true | _ -> false
let is_revert exp = match exp with Lv (Var ("revert", _)) -> true | _ -> false
let is_this exp = match exp with Lv (Var ("this", _)) -> true | _ -> false

(***********************)
(***********************)
(***** Other Utils *****)
(***********************)
(***********************)

let rec replace_exp (tar : exp) (rep : exp) (exp : exp) : exp =
  if to_string_exp exp = to_string_exp tar then rep
  else
    match exp with
    | V _ | Str _ -> exp
    | Lv lv -> Lv (replace_lv tar rep lv)
    | Cast (typ, e) -> Cast (typ, replace_exp tar rep e)
    | BinOp (bop, e1, e2, einfo) ->
      BinOp (bop, replace_exp tar rep e1, replace_exp tar rep e2, einfo)
    | UnOp (uop, e, typ) -> UnOp (uop, replace_exp tar rep e, typ)
    | Ite (i, t, e, typ) ->
      Ite (replace_exp tar rep i, replace_exp tar rep t, replace_exp tar rep e, typ)
    | ETypeName _ -> exp
    | IndexRangeAccess (base, sop, fop, einfo) ->
      let base' = replace_lv tar rep base in
      let sop' = replace_eop tar rep sop in
      let fop' = replace_eop tar rep fop in
      IndexRangeAccess (base', sop', fop', einfo)
    | TypeInfo _ -> exp
    | IncTemp (e, b, loc) -> IncTemp (replace_exp tar rep e, b, loc)
    | DecTemp (e, b, loc) -> DecTemp (replace_exp tar rep e, b, loc)
    | CallTemp (e, args, ethop, gasop, einfo) ->
      let e' = replace_exp tar rep e in
      let args' = List.map (replace_exp tar rep) args in
      let ethop' = replace_eop tar rep ethop in
      let gasop' = replace_eop tar rep gasop in
      CallTemp (e', args', ethop', gasop', einfo)
    | AssignTemp (lv, e, loc) -> AssignTemp (replace_lv tar rep lv, replace_exp tar rep e, loc)
    | CondTemp (e1, e2, e3, typ, loc) ->
      let e1' = replace_exp tar rep e1 in
      let e2' = replace_exp tar rep e2 in
      let e3' = replace_exp tar rep e3 in
      CondTemp (e1', e2', e3', typ, loc)

and replace_eop tar rep eop = Option.map (replace_exp tar rep) eop

and replace_lv tar rep lv =
  match lv with
  | Var _ -> lv
  | MemberAccess (e, x, xinfo, typ) -> MemberAccess (replace_exp tar rep e, x, xinfo, typ)
  | IndexAccess (e, eop, typ) -> IndexAccess (replace_exp tar rep e, replace_eop tar rep eop, typ)
  | Tuple (eoplst, typ) -> Tuple (List.map (replace_eop tar rep) eoplst, typ)
;;

let replace_lvop tar rep lvop = Option.map (replace_lv tar rep) lvop

let rec replace_stmt tar rep (stmt : Stmt.t) : Stmt.t =
  match stmt with
  | Assign (lv, e, loc) -> Assign (replace_lv tar rep lv, replace_exp tar rep e, loc)
  | Decl _ -> stmt
  | Call (lvop, e, args, ethop, gasop, loc) ->
    let lvop' = replace_lvop tar rep lvop in
    let e' = replace_exp tar rep e in
    let args' = List.map (replace_exp tar rep) args in
    let ethop' = replace_eop tar rep ethop in
    let gasop' = replace_eop tar rep gasop in
    Call (lvop', e', args', ethop', gasop', loc)
  | Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc } ->
    let target = replace_exp tar rep target in
    let args = List.map (replace_exp tar rep) args in
    let ether = replace_eop tar rep ether in
    let gas = replace_eop tar rep gas in
    Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc }
  | Skip -> stmt
  | Return (eop, loc) -> Return (replace_eop tar rep eop, loc)
  | Revert -> stmt
  | Assume (e, loc) -> Assume (replace_exp tar rep e, loc)
  | Assert (e, vtyp, loc) -> Assert (replace_exp tar rep e, vtyp, loc)
  | If (e, s1, s2, i) ->
    If (replace_exp tar rep e, replace_stmt tar rep s1, replace_stmt tar rep s2, i)
  | Seq (s1, s2) -> Seq (replace_stmt tar rep s1, replace_stmt tar rep s2)
  | While (e, s) -> While (replace_exp tar rep e, replace_stmt tar rep s)
  | Assembly _ -> stmt
  | Break | Continue | Placeholder -> stmt
  | Unchecked (lst, loc) -> Unchecked (List.map (replace_stmt tar rep) lst, loc)
  | Label _ -> stmt
;;

exception NoParameters

let params_to_lv params =
  if List.length params = 1 then
    let x, vinfo = List.hd params in
    Var (x, vinfo)
  else if List.length params > 1 then
    let eops = List.map (fun (x, vinfo) -> Some (Lv (Var (x, vinfo)))) params in
    let tuple_typ = Typ.TupleType (List.map (fun (_, vinfo) -> vinfo.vtyp) params) in
    Tuple (eops, tuple_typ)
  else raise NoParameters
;;

let args_to_exp (args : exp list) : exp =
  if List.length args = 1 then List.hd args
  else if List.length args > 1 then
    let eops = List.map (fun e -> Some e) args in
    let tuple_typ = Typ.TupleType (List.map get_type_exp args) in
    Lv (Tuple (eops, tuple_typ))
  else raise NoParameters
;;

let blk_keyword_vars =
  [
    "block.basefee";
    "block.chainid";
    "block.coinbase";
    "block.difficulty";
    "block.gaslimit";
    "block.number";
    "block.timestamp";
    "now";
  ]
;;

let keyword_vars =
  blk_keyword_vars
  @ [
      "msg.data";
      "msg.data.length";
      "msg.sender";
      "msg.value";
      "msg.gas";
      "msg.sig";
      "this";
      "tx.gasprice";
      "tx.origin";
    ]
;;

let is_balance_keyword lv =
  match lv with
  | MemberAccess (e, id, _, _)
    when (Typ.is_address (get_type_exp e) || Typ.is_contract (get_type_exp e))
         && String.equal id "balance" ->
    true
  | _ -> false
;;

let init_funcs =
  [ "array_init"; "dbytes_init"; "string_init"; "contract_init"; "struct_init"; "struct_init2" ]
;;

(** {b Note} suicide is disallowed since solc 0.5.0 *)
let built_in_funcs =
  [
    "abi.encode";
    "abi.decode";
    "abi.encodePacked";
    "abi.encodeWithSignature";
    "abi.encodeWithSelector";
    "revert";
    "keccak256";
    "sha3";
    "sha256";
    "ripemd160";
    "delete";
    "selfdestruct";
    "suicide";
    "ecrecover";
    "addmod";
    "mulmod";
    "blockhash";
    "block.blockhash";
    "gasleft";
  ]
;;

let max_256bit = Z.(sub (pow (of_int 2) 256) one)
let max_of_n_bits n = Z.(sub (pow (of_int 2) n) one)

let rec var_lv ?(fid = false) (lv : lv) : Var.t Set.t =
  match lv with
  | Var (x, xinfo) -> Set.singleton (x, xinfo.vtyp)
  | MemberAccess (e, x, xinfo, _) -> Set.add (x, xinfo.vtyp) (var_exp ~fid e)
  | IndexAccess (e1, Some e2, _) -> Set.union (var_exp ~fid e1) (var_exp ~fid e2)
  | IndexAccess (e, None, _) -> var_exp ~fid e
  | Tuple (eops, _) ->
    List.fold_left (fun acc eop -> Set.union (var_eop ~fid eop) acc) Set.empty eops

and var_exp ?(fid = false) (exp : exp) : Var.t Set.t =
  match exp with
  | V _ | Str _ -> Set.empty
  | Lv lv ->
    if List.mem (to_string_lv lv) keyword_vars then Set.singleton (to_string_lv lv, get_type_lv lv)
    else var_lv ~fid lv
  | Cast (_, e) -> var_exp ~fid e
  | BinOp (_, e1, e2, _) -> Set.union (var_exp ~fid e1) (var_exp ~fid e2)
  | UnOp (_, e, _) -> var_exp ~fid e
  | Ite (i, t, e, _) -> Set.union (Set.union (var_exp ~fid i) (var_exp ~fid t)) (var_exp ~fid e)
  | ETypeName _ -> Set.empty
  | IndexRangeAccess (base, sop, fop, _) ->
    Set.union (var_lv base) (Set.union (var_eop ~fid sop) (var_eop ~fid fop))
  | TypeInfo _ -> Set.empty
  | IncTemp (e, _, _) | DecTemp (e, _, _) -> var_exp ~fid e
  | CallTemp (e, exps, ethop, gasop, _) -> var_call ~fid (None, e, exps, ethop, gasop)
  | CondTemp (e1, e2, e3, _, _) ->
    Set.union (var_exp ~fid e1) (Set.union (var_exp ~fid e2) (var_exp ~fid e3))
  | AssignTemp (lv, e, _) -> Set.union (var_lv ~fid lv) (var_exp ~fid e)

and var_eop ?(fid = false) eop = match eop with Some e -> var_exp ~fid e | None -> Set.empty

and var_call ?(fid = false) (lvop, e, exps, ethop, gasop) =
  let set1 = match lvop with None -> Set.empty | Some lv -> var_lv ~fid lv in
  let set2 =
    match e with
    | e when List.mem (to_string_exp e) built_in_funcs -> Set.empty
    | Lv (Var (fname, vinfo)) when fid -> Set.singleton (fname, vinfo.vtyp)
    | Lv (MemberAccess (Lv (Var (v, vinfo)), _, _, _))
    (* safemath.add(...) *)
      when Typ.is_contract vinfo.vtyp || v = "super" ->
      Set.empty
    | Lv (MemberAccess (arg, _, _, _)) ->
      (* x.add(...), x[y].add(...) *)
      var_exp ~fid arg
    | _ -> Set.empty
  in
  let set3 = List.fold_left (fun acc e' -> Set.union (var_exp ~fid e') acc) Set.empty exps in
  let set4 = var_eop ~fid ethop in
  let set5 = var_eop ~fid gasop in
  Set.union set1 (Set.union set2 (Set.union set3 (Set.union set4 set5)))
;;

let[@warning "-8"] var_extcall ?(fid = false)
    (Stmt.Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc }) =
  ignore (id, fkey, is_static, loc);
  let set1 = Option.map (var_lv ~fid) ret |? Set.empty in
  let set2 = var_exp ~fid target in
  let set3 = List.fold_left (fun acc e' -> Set.union (var_exp ~fid e') acc) Set.empty args in
  let set4 = var_eop ~fid ether in
  let set5 = var_eop ~fid gas in
  Set.union set1 set2 |> Set.union set3 |> Set.union set4 |> Set.union set5
;;

let rec var_stmt ?(fid = false) (stmt : Stmt.t) : Var.t Set.t =
  match stmt with
  | Assign (lv, exp, _) -> Set.union (var_lv ~fid lv) (var_exp ~fid exp)
  | Decl lv -> var_lv ~fid lv
  | Call (lvop, e, exps, ethop, gasop, _) -> var_call ~fid (lvop, e, exps, ethop, gasop)
  | Extcall c -> var_extcall (Extcall c)
  | Skip -> Set.empty
  | Return (None, _) -> Set.empty
  | Return (Some e, _) -> var_exp ~fid e
  | Revert -> Set.empty
  | Assume (e, _) | Assert (e, _, _) -> var_exp ~fid e
  | Assembly _ -> Set.empty
  | If (e, s1, s2, _) ->
    let set1 = var_exp ~fid e in
    let set2 = var_stmt ~fid s1 in
    let set3 = var_stmt ~fid s2 in
    Set.union set1 (Set.union set2 set3)
  | Seq (s1, s2) -> Set.union (var_stmt ~fid s1) (var_stmt ~fid s2)
  | While (e, s) -> Set.union (var_exp ~fid e) (var_stmt ~fid s)
  | Break | Continue | Placeholder -> Set.empty
  | Unchecked (lst, _) ->
    List.fold_left (fun acc s -> Set.union (var_stmt ~fid s) acc) Set.empty lst
  | Label _ -> Set.empty
;;

let var_func ?(fid = false) (func : Func.t) : Var.t Set.t =
  let mk params = params |> List.map (T2.uncurry Var2.mk) |> List.map Var2.to_var |> Set.of_list in
  let s1 = mk func.params in
  let s2 = mk func.ret_params in
  let s3 = var_stmt ~fid func.body in
  Set.union s1 s2 |> Set.union s3
;;

let var_contract ?(fid = false) (c : Contract.t) : Var.t Set.t =
  let s1 = c.decls |> List.map State_var_decl.to_var |> Set.of_list in
  let s2 = c.funcs |> List.map (var_func ~fid) |> List.fold_left Set.union Set.empty in
  Set.union s1 s2
;;

let rec int_lv (lv : lv) : Z.Set.t =
  match lv with
  | Var _ -> Z.Set.empty
  | MemberAccess (e, _, _, _) -> int_exp e
  | IndexAccess (e1, Some e2, _) -> Z.Set.union (int_exp e1) (int_exp e2)
  | IndexAccess (e, None, _) -> int_exp e
  | Tuple (eops, _) ->
    List.fold_left
      (fun acc eop -> match eop with None -> acc | Some e -> Z.Set.union (int_exp e) acc)
      Z.Set.empty eops

and int_exp (exp : exp) : Z.Set.t =
  match exp with
  | V (Int n) -> Z.Set.singleton n
  | V _ | Str _ -> Z.Set.empty
  | Lv lv -> int_lv lv
  | Cast (_, e) -> int_exp e
  | BinOp (_, e1, e2, _) -> Z.Set.union (int_exp e1) (int_exp e2)
  | UnOp (_, e, _) -> int_exp e
  | ETypeName _ -> Z.Set.empty
  | _ -> failwith "int_exp: temp expressions encountered"
;;

let zpow a b = Big_int_Z.power_big a b

(* currently, casting is performed in the vc generation step. *)
let rec folding (exp : exp) : exp =
  match exp with
  | V (Int n) -> V (Int n)
  | BinOp (Add, V (Int n1), V (Int n2), _) -> V (Int (Z.add n1 n2))
  | BinOp (Sub, V (Int n1), V (Int n2), _) -> V (Int (Z.sub n1 n2))
  | BinOp (Mul, V (Int n1), V (Int n2), _) -> V (Int (Z.mul n1 n2))
  | BinOp (Div, V (Int n1), V (Int n2), _) -> V (Int (Z.div n1 n2))
  | BinOp (Mod, V (Int n1), V (Int n2), _) -> V (Int (Z.rem n1 n2))
  | BinOp (Exponent, V (Int n1), V (Int n2), _) -> V (Int (zpow n1 n2))
  | BinOp (bop, e1, e2, einfo) -> BinOp (bop, folding e1, folding e2, einfo)
  | _ -> failwith "folding"
;;

let rec constant_folding (exp : exp) : exp =
  let exp' = folding exp in
  if String.equal (to_string_exp exp) (to_string_exp exp') then exp' else constant_folding exp'
;;

let common_typ : exp -> exp -> Typ.t =
 fun e1 e2 ->
  let t1, t2 = (get_type_exp e1, get_type_exp e2) in
  if t1 = t2 then t1
  else
    match (t1, t2) with
    | ConstInt, EType (UInt n) ->
      let n' = bit_unsigned_of_int (get_bigint (constant_folding e1)) 8 in
      EType (UInt (max n n'))
    | EType (UInt n), ConstInt ->
      let n' = bit_unsigned_of_int (get_bigint (constant_folding e2)) 8 in
      EType (UInt (max n n'))
    | ConstInt, EType (SInt n) ->
      let n' = bit_signed_of_int (get_bigint (constant_folding e1)) 8 in
      EType (SInt (max n n'))
    | EType (SInt n), ConstInt ->
      let n' = bit_signed_of_int (get_bigint (constant_folding e2)) 8 in
      EType (SInt (max n n'))
    | _ -> Typ.preceding t1 t2
;;

let mk_einfo (t : Typ.t) : Einfo.t = { loc = Loc.dummy; typ = t; id = -1 }

let mk_finfo ?(vis = Vis.Public) (c : Contract.t) : Finfo.t =
  {
    is_constructor = false;
    is_payable = false;
    is_modifier = false;
    mod_list = [];
    mod_list2 = [];
    (* modifier by inheritance *)
    param_loc = Loc.dummy;
    ret_param_loc = Loc.dummy;
    fvis = vis;
    mutability = NonPayable;
    fid = -1;
    floc = Loc.dummy;
    blk_loc = Loc.dummy;
    scope = c.info.numid;
    scope_s = c.name;
    org_scope_s = c.name;
    cfg = Cfg.empty;
  }
;;

let mk_index_access (e1 : exp) (e2 : exp) : exp =
  Lv (IndexAccess (e1, Some e2, Typ.range (get_type_exp e1)))
;;

let mk_member_access (e : exp) ((x, t) : Var.t) : exp =
  Lv (MemberAccess (e, x, mk_vinfo ~typ:t (), t))
;;

let mk_eq e1 e2 = BinOp (Eq, e1, e2, mk_einfo (EType Bool))
let mk_neq e1 e2 = BinOp (NEq, e1, e2, mk_einfo (EType Bool))
let mk_ge e1 e2 = BinOp (GEq, e1, e2, mk_einfo (EType Bool))
let mk_gt e1 e2 = BinOp (Gt, e1, e2, mk_einfo (EType Bool))

let mk_and e1 e2 =
  let _ = assert (Typ.is_bool (get_type_exp e1)) in
  let _ = assert (Typ.is_bool (get_type_exp e2)) in
  BinOp (LAnd, e1, e2, mk_einfo (EType Bool))
;;

let mk_or e1 e2 =
  let _ = assert (Typ.is_bool (get_type_exp e1)) in
  let _ = assert (Typ.is_bool (get_type_exp e2)) in
  BinOp (LOr, e1, e2, mk_einfo (EType Bool))
;;

let mk_add e1 e2 = BinOp (Add, e1, e2, mk_einfo (common_typ e1 e2))
let mk_sub e1 e2 = BinOp (Sub, e1, e2, mk_einfo (common_typ e1 e2))
let mk_mul e1 e2 = BinOp (Mul, e1, e2, mk_einfo (common_typ e1 e2))
let mk_div e1 e2 = BinOp (Div, e1, e2, mk_einfo (common_typ e1 e2))
let mk_not e = UnOp (LNot, e, EType Bool)

(** rename local variables with given labels *)
let rec rename_lv ?(reent = false) (label : string) (global_vars : Var.t list) (lv : lv) : lv =
  let re = rename_e ~reent label global_vars in
  match lv with
  | Var (x, xinfo) ->
    (* global variables & pseudo-global variables are not renamed *)
    (* memo variables are not renamed, they are not interfunctional, we can feel free to destroy *)
    if
      List.mem (x, xinfo.vtyp) global_vars
      || List.mem x [ "@TU"; "@Invest"; "@Invest_sum" ]
      || String.ends_with x "@MEMO"
    then lv
    else Var (x ^ label, xinfo)
  | MemberAccess (_, _, _, typ) when Typ.is_enum typ -> lv
  | MemberAccess (e, x, xinfo, typ) -> MemberAccess (re e, x, xinfo, typ)
  | IndexAccess (_, None, _) -> failwith "rename_lv"
  | IndexAccess (e1, Some e2, typ) -> IndexAccess (re e1, Some (re e2), typ)
  | Tuple (eoplst, typ) ->
    let eoplst' =
      List.map (fun eop -> match eop with None -> None | Some e -> Some (re e)) eoplst
    in
    Tuple (eoplst', typ)

and rename_e ?(reent = false) : string -> Var.t list -> exp -> exp =
 fun lab gvars exp ->
  match exp with
  | V _ | Str _ -> exp
  | Lv lv ->
    let str = to_string_lv lv in
    if reent && str = "msg.sender" then
      let vinfo = mk_vinfo ~typ:(EType Address) ~org:(Some exp) () in
      Lv (Var ("msg.sender" ^ lab, vinfo))
    else if List.mem str keyword_vars || str = "abi" then exp
    else Lv (rename_lv ~reent lab gvars lv)
  | Cast (typ, e) -> Cast (typ, rename_e ~reent lab gvars e)
  | BinOp (bop, e1, e2, einfo) ->
    BinOp (bop, rename_e ~reent lab gvars e1, rename_e ~reent lab gvars e2, einfo)
  | UnOp (uop, e, typ) -> UnOp (uop, rename_e ~reent lab gvars e, typ)
  | Ite (i, t, e, typ) ->
    Ite (rename_e ~reent lab gvars i, rename_e ~reent lab gvars t, rename_e ~reent lab gvars e, typ)
  | ETypeName _ -> exp
  | IndexRangeAccess (base, sop, fop, einfo) ->
    let rename_eop eop =
      match eop with Some e -> Some (rename_e ~reent lab gvars e) | None -> None
    in
    IndexRangeAccess (rename_lv ~reent lab gvars base, rename_eop sop, rename_eop fop, einfo)
  | TypeInfo _ -> exp
  | IncTemp _ | DecTemp _ | CallTemp _ | CondTemp _ | AssignTemp _ -> failwith "rename_e"
;;

let rec rename_stmt ?(reent = false) : string -> Var.t list -> string list -> Stmt.t -> Stmt.t =
 fun lab gvars cnames stmt ->
  match stmt with
  | Assign (lv, e, loc) -> Assign (rename_lv ~reent lab gvars lv, rename_e ~reent lab gvars e, loc)
  | Decl lv -> Decl (rename_lv ~reent lab gvars lv)
  | Call (lvop, e, args, ethop, gasop, loc) ->
    let lvop' = match lvop with None -> lvop | Some lv -> Some (rename_lv ~reent lab gvars lv) in
    let e' =
      match e with
      (* rename only for contract object cases *)
      | Lv (MemberAccess ((Lv (Var (x, _)) as obj), fname, fname_info, typ)) ->
        if List.mem x cnames || x = "super" then e (* static call *)
        else Lv (MemberAccess (rename_e ~reent lab gvars obj, fname, fname_info, typ))
      | _ -> e (* built-in functions, static call without prefixes *)
    in
    let args' =
      if to_string_exp e = "struct_init" || to_string_exp e = "contract_init" then
        (* the first arg is struct/contract name; see preprocess.ml *)
        List.hd args :: List.map (rename_e ~reent lab gvars) (List.tl args)
      else List.map (rename_e ~reent lab gvars) args
    in
    let ethop' = match ethop with None -> ethop | Some e -> Some (rename_e ~reent lab gvars e) in
    let gasop' = match gasop with None -> gasop | Some e -> Some (rename_e ~reent lab gvars e) in
    Call (lvop', e', args', ethop', gasop', loc)
  | Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc } ->
    let ret = match ret with None -> ret | Some lv -> Some (rename_lv ~reent lab gvars lv) in
    let target = rename_e ~reent lab gvars target in
    let args = List.map (rename_e ~reent lab gvars) args in
    let ether = match ether with None -> ether | Some e -> Some (rename_e ~reent lab gvars e) in
    let gas = match gas with None -> gas | Some e -> Some (rename_e ~reent lab gvars e) in
    Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc }
  | Skip -> stmt
  | Return (None, _) -> stmt
  | Return (Some e, loc) -> Return (Some (rename_e ~reent lab gvars e), loc)
  | Revert -> stmt
  | Assume (e, loc) -> Assume (rename_e ~reent lab gvars e, loc)
  | Assert (e, vtyp, loc) -> Assert (rename_e ~reent lab gvars e, vtyp, loc)
  | Assembly (lst, loc) ->
    let gnames = List.map fst gvars in
    let lst' =
      List.map (fun (x, refid) -> if List.mem x gnames then (x, refid) else (x ^ lab, refid)) lst
    in
    Assembly (lst', loc)
  | If (e, s1, s2, i) ->
    If
      ( rename_e ~reent lab gvars e,
        rename_stmt ~reent lab gvars cnames s1,
        rename_stmt ~reent lab gvars cnames s2,
        i )
  | Seq (s1, s2) ->
    Seq (rename_stmt ~reent lab gvars cnames s1, rename_stmt ~reent lab gvars cnames s2)
  | While (e, s) -> While (rename_e ~reent lab gvars e, rename_stmt ~reent lab gvars cnames s)
  | Break | Continue | Placeholder -> stmt
  | Unchecked (lst, loc) ->
    let lst' = List.map (rename_stmt ~reent lab gvars cnames) lst in
    Unchecked (lst', loc)
  | Label _ -> stmt
;;

let no_eth_gas_modifiers (stmt : Stmt.t) =
  match stmt with
  | Call (_, _, _, None, None, _) -> true
  | Call _ -> false
  | _ -> failwith "no_eth_gas_modifiers"
;;

(* counters *)
open struct
  module Counter_tmpvar = Counter.M ()
  module Counter_nondet_selector = Counter.M ()
  module Counter_extcall_id = Counter.M ()
end

let tmpvar = "@Tmp"
let nondet_selector_name = "@Nondet"

(** Make a new temporal variable. Thread safe. *)
let gen_tmpvar ?(org = None) ?(loc = -1) typ =
  let id = tmpvar ^ Int.to_string (Counter_tmpvar.gen ()) in
  let vinfo = mk_vinfo ~typ ~org ~loc:Loc.(mk ~line:loc ~finish_line:loc ()) () in
  Var (id, vinfo)
;;

(** Similar with {!gen_tmpvar}, but specifies by name that it is used when modeling a
    nondeterministic selection of VeriSmart. Thread safe. *)
let gen_nondet_selector () =
  let id = nondet_selector_name ^ Int.to_string (Counter_nondet_selector.gen ()) in
  let vinfo = mk_vinfo ~typ:(EType Bool) () in
  Var (id, vinfo)
;;

let mk_extcall_id () = Counter_extcall_id.gen ()

let rec get_extcall_ids (stmt : Stmt.t) =
  match stmt with
  | Extcall e -> [ e.id ]
  | Seq (s1, s2) -> get_extcall_ids s1 @ get_extcall_ids s2
  | _ -> []
;;
