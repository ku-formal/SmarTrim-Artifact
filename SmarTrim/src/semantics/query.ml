open Vocab
open Vocab.Batteries
open Vlang
open! Frontend
open Frontend.Lang

module Status = struct
  type t = Proven | Unproven | Disproven | Unfalsifiable [@@deriving show { with_path = false }]
end

type origin = Org_stmt of Stmt.t | Org_exp of exp | Org_func of Fkey.t

type t = {
  vc : vformula;  (** safety propery of a query per path. *)
  vc2 : vformula;  (** vc for generating lasting inputs. *)
  vc_kind : [ `Basic | `Delayed ];
  kind : Kind.t;
  qloc : Loc.t;
  org_q : origin;  (** original expression in the source code *)
  path : Paths.t;  (** basic path wherein vc was generated *)
  src_f : Fkey.t;  (** function signature where vc was generated *)
  sc_src : string;  (** safety condition at source-code level *)
  attacker_src : string;
      (** attacker (ether receiver) at source-code level; only valid for EL detection *)
  eth_src : string;
      (** weis to be transferred to attacker at source-code level; only valid for EL detection *)
}
[@@deriving fields ~fields]

let code_transfer_sender_has_enough_money = -100
let code_transfer_sender_bal_dec = -99
let code_transfer_recipient_bal_inc = -98
let code_transferFrom_from_bal_enough = -97
let code_transferFrom_sender_allow_enough = -96
let code_transferFrom_from_bal_dec = -95
let code_transferFrom_to_bal_inc = -94
let code_transferFrom_sender_allow_dec = -93
let code_approve_set = -92
let code_balance_sum_no_overflow = -91
let code_total_ge_balance = -90
let code_public_getter = Preprocess.code_public_getter

let to_string_origin ?(report = false) (org : origin) : string =
  match org with
  | Org_stmt s -> to_string_stmt ~report s
  | Org_exp e -> to_string_exp ~report e
  | Org_func f -> Fkey.to_string f
;;

let compare (q1 : t) (q2 : t) : int =
  if Stdlib.compare q1.kind q2.kind = 0 then
    if Stdlib.compare q1.qloc q2.qloc = 0 then
      String.compare
        (to_string_origin ~report:true q1.org_q)
        (to_string_origin ~report:true q2.org_q)
    else Stdlib.compare q1.qloc q2.qloc
  else Stdlib.compare q1.kind q2.kind
;;

let sort (qs : t list) : t list = List.sort compare qs

(** group queries that have the same line numbers and same expressions in original source code *)
let group (qs : t list) : t list list = List.group compare qs

(** Query ID. *)
module ID = struct
  type t = { kind : Kind.t; loc : Loc.t; exp : string [@compare.ignore] } [@@deriving show, compare]

  let show_for_stdout ({ kind = _; loc; exp } : t) =
    if loc.line > 0 then Printf.sprintf "line %i, %s" loc.line exp
    else begin
      let l = loc.line in
      if l = code_transfer_sender_has_enough_money then
        "[transfer] The message sender should have enough tokens"
      else if l = code_transfer_sender_bal_dec then
        "[transfer] The sender's balance should decrease properly"
      else if l = code_transfer_recipient_bal_inc then
        "[transfer] The recipient's balance should increase properly"
      else if l = code_transferFrom_from_bal_enough then
        "[transferFrom] The from's balance should have enough tokens"
      else if l = code_transferFrom_sender_allow_enough then
        "[transferFrom] The sender's allowance should be enough"
      else if l = code_transferFrom_from_bal_dec then
        "[transferFrom] The from's balance should decrease properly"
      else if l = code_transferFrom_to_bal_inc then
        "[transferFrom] The recipient's balance should increase properly"
      else if l = code_transferFrom_sender_allow_dec then
        "[transferFrom] The sender's allowance should decrease properly"
      else if l = code_approve_set then "[approve] The allowance should be set properly"
      else if l = code_balance_sum_no_overflow then
        "[invariant] sum of balances should not overflow"
      else if l = code_total_ge_balance then
        "[invariant] totalSupply should be greater than or equal to any balances"
      else failwith "Unexpected assertion code"
    end
  ;;

  let show_for_machine ({ kind; loc; exp } : t) = Pp.spf "%s:%i:%s" (Kind.show kind) loc.line exp

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

let get_qid (q : t) : ID.t =
  { kind = q.kind; loc = q.qloc; exp = to_string_origin ~report:true q.org_q }
;;

let locstring_to_line (l : string) = String.split_on_char ':' l |> List.hd |> int_of_string
let line2loc (line : int) : Loc.t = { line; finish_line = line; offset = 0; len = 0 }
let choose_final_vc (q : t) = match q.vc_kind with `Basic -> q.vc | `Delayed -> q.vc2
