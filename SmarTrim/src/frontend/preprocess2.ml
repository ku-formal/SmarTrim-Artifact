open Lang
open Options

(***********************************************************)
(*** Insert assertions for detecting invalid assignments ***)
(***********************************************************)

let pid = ref 0

let fresh_pid () =
  incr pid;
  !pid
;;

let is_pseudo_stmt_node (n : Node.t) (g : Cfg.t) : bool =
  let stmt = Cfg.find_stmt n g in
  match stmt with
  | Assign (lv, e, _) ->
    Set.exists (fun x -> String.starts_with (fst x) "@") (Set.union (var_lv lv) (var_exp e))
  | Assume (e, _) -> Set.exists (fun x -> String.starts_with (fst x) "@") (var_exp e)
  | Assert (_, "assign_const", _) -> true
  | Assert (_, "no_effect", _) -> true
  | Assert (_, "deadcode", _) -> true
  | _ -> false
;;

let rec get_addrs (stmt : Stmt.t) : exp list =
  match stmt with
  | Assign (IndexAccess (Lv (Var (_x, xinfo)), Some idx, _), _, _)
    when Typ.is_usual_mapping xinfo.vtyp ->
    [ idx ]
  | Seq (s1, s2) -> get_addrs s1 @ get_addrs s2
  | If (_e, s1, s2, _) -> get_addrs s1 @ get_addrs s2
  | While (_e, s) -> get_addrs s
  | _ -> []
;;

let all_addrs_diff (addrs : exp list) : exp =
  let is_true e = match e with V (Bool true) -> true | _ -> false in
  let pairs = List.cartesian_product addrs addrs in
  (* avoid vacuously true (e.g., a!=a) case, and redundant (e.g., a!=b, b!=a) condition *)
  let pairs = List.filter (fun (a1, a2) -> to_string_exp a1 < to_string_exp a2) pairs in
  List.fold_left
    (fun acc (a1, a2) ->
      let res = mk_neq a1 a2 in
      if is_true acc then res else mk_and acc res)
    (V (Bool true)) pairs
;;

let gen_assign_const all lv loc : Stmt.t =
  let base = mk_eq (Lv lv) (V (Int Z.zero)) in
  let addrs =
    get_addrs all
    |> List.sort_uniq (fun e1 e2 -> Stdlib.compare (to_string_exp e1) (to_string_exp e2))
  in
  if List.length addrs = 0 || List.length addrs = 1 then Assert (base, "assign_const", loc)
  else
    let diff = all_addrs_diff addrs in
    Assert (mk_or (mk_not diff) base, "assign_const", loc)
;;

let rec invalid_assign_s (cnames : string list) (fmap : FuncMap.t) (all : Stmt.t) (stmt : Stmt.t) :
    Stmt.t =
  match stmt with
  | Assign (lv, _, _) when String.starts_with (to_string_lv lv) Translator.param_name ->
    stmt (* exclude assignments from return normalization *)
  | Assign (lv, BinOp (bop, _, _, _), loc) when (bop = Add || bop = Sub) && loc.line > 0 ->
    (* loc > 0: exclude modelled assignment *)
    let pre = "@pre" ^ string_of_int (fresh_pid ()) in
    (* org: to maintain original name even when inlined; affecting the performance (query normalization) *)
    let org = Lv (Var (pre, mk_vinfo ~typ:(get_type_lv lv) ())) in
    let pre = Var (pre, mk_vinfo ~typ:(get_type_lv lv) ~org:(Some org) ()) in
    let assign_pre = Stmt.Assign (pre, Lv lv, loc) in
    let check1 = gen_assign_const all lv loc in
    let check2 = Stmt.Assert (mk_eq (Lv pre) (Lv lv), "no_effect", loc) in
    Seq (assign_pre, Seq (stmt, Seq (check1, check2)))
  | Assign (_lv, e, _) when Typ.is_const_int (get_type_exp e) ->
    stmt (* for no-effect checker, exclude constant assignments *)
  | Assign (lv, _e, loc)
    when (Typ.is_uintkind (get_type_lv lv)
         || Typ.is_sintkind (get_type_lv lv)
         || Typ.is_address_kind (get_type_lv lv))
         && loc.line > 0 ->
    let pre = "@pre" ^ string_of_int (fresh_pid ()) in
    let org = Lv (Var (pre, mk_vinfo ~typ:(get_type_lv lv) ())) in
    let pre = Var (pre, mk_vinfo ~typ:(get_type_lv lv) ~org:(Some org) ()) in
    let assign_pre = Stmt.Assign (pre, Lv lv, loc) in
    let check = Stmt.Assert (mk_eq (Lv pre) (Lv lv), "no_effect", loc) in
    Seq (assign_pre, Seq (stmt, check))
  | Decl _ -> stmt
  | Seq (s1, s2) -> Seq (invalid_assign_s cnames fmap all s1, invalid_assign_s cnames fmap all s2)
  | If ((BinOp (_bop, _e1, _e2, einfo) as e), s1, s2, i) when einfo.loc.line > 0 ->
    let check = Stmt.Assert (V (Bool false), "deadcode", einfo.loc) in
    Seq (If (e, invalid_assign_s cnames fmap all s1, invalid_assign_s cnames fmap all s2, i), check)
  | If (e, s1, s2, i) ->
    If (e, invalid_assign_s cnames fmap all s1, invalid_assign_s cnames fmap all s2, i)
  | While (e, s) -> While (e, invalid_assign_s cnames fmap all s) (* built-in functions *)
  | Call _ when FuncMap.is_undef_call fmap stmt -> stmt
  | Call (_lvop, _e, _args, _ethop, _gasop, loc) when FuncMap.is_internal_call fmap cnames stmt ->
    let check = Stmt.Assert (V (Bool false), "deadcode", loc) in
    Seq (stmt, check)
  | Call (_lvop, e, _args, _ethop, _gasop, _loc) ->
    let _ = match e with Lv (MemberAccess (rcv, _, _, _)) -> rcv | _ -> assert false in
    stmt
  | _ -> stmt
;;

(**************************************************)
(*** Desugar built-in functions that send money ***)
(**************************************************)

let msg_sender = Var ("msg.sender", mk_vinfo ~typ:(EType Address) ())
let msg_value = Var ("msg.value", mk_vinfo ~typ:(EType (UInt 256)) ())
let trust_map = Var ("@TU", mk_vinfo ~typ:(Mapping (Address, EType Bool)) ())
let invest_sum = Var ("@Invest_sum", mk_vinfo ~typ:(EType (UInt 256)) ())

(** Memo the invest sum before payment. Used to generate VCs. Considered as a local variable. *)
let invest_sum_memo = Var ("@Invest_sum@MEMO", mk_vinfo ~typ:(EType (UInt 256)) ())

(** Memo the pay amount. Used to generate VCs. Considered as a local variable. *)
let pay_amount_memo = Var ("@Pay_amount@MEMO", mk_vinfo ~typ:(EType (UInt 256)) ())

let this_info main_contract = mk_vinfo ~typ:(EType (Contract main_contract)) ()
let this main_contract = Lv (Var ("this", this_info main_contract))
let this_address main_contract = Cast (EType Address, this main_contract)
let balance_info = mk_vinfo ~typ:(EType (UInt 256)) ()

let this_balance main_contract =
  MemberAccess (this_address main_contract, "balance", balance_info, EType (UInt 256))
;;

(** desugars [address.call()], [address.transfer()], etc *)
let rec convert_call_s (cnames : string list) (fmap : FuncMap.t) (stmt : Stmt.t) :
    Stmt.t * Extcall.t list =
  let this_balance = this_balance !main_contract in
  let balance_require_stmt eth =
    let cond1 = mk_ge (Lv this_balance) eth in
    Stmt.Assume (cond1, Loc.dummy)
  in
  let balance_stmt target eth =
    let not_trusted =
      mk_eq (Lv (IndexAccess (Lv trust_map, Some target, EType Bool))) (V (Bool false))
    in
    let memo_invest_sum = Stmt.Assign (invest_sum_memo, Lv invest_sum, Loc.dummy) in
    let eth' =
      match eth with
      | BinOp (Mul, Lv v1, Lv (Var (v, _)), _)
        when Options.Mode.(!mode = Verify) && String.starts_with_stdlib v ~prefix:"sellPrice" ->
        Lv v1
      | _ -> eth
    in
    let memo_pay_amount = Stmt.Assign (pay_amount_memo, eth', Loc.dummy) in
    let invest_sum_stmt =
      Stmt.Seq
        ( Seq (memo_invest_sum, memo_pay_amount),
          If
            ( not_trusted,
              Assign (invest_sum, mk_sub (Lv invest_sum) (Lv pay_amount_memo), Loc.dummy),
              Skip,
              Ifinfo.dummy ) )
    in
    let target =
      if Typ.is_address_kind (get_type_exp target) then target else Cast (EType Address, target)
    in
    let rcv_balance = MemberAccess (target, "balance", balance_info, EType (UInt 256)) in
    (* we do not check addition overflow since eth will be very small wrt 2^256 *)
    let pay_stmt =
      Stmt.Seq
        ( Assign (this_balance, mk_sub (Lv this_balance) (Lv pay_amount_memo), Loc.dummy),
          Assign (rcv_balance, mk_add (Lv rcv_balance) (Lv pay_amount_memo), Loc.dummy) )
    in
    Stmt.Seq (invest_sum_stmt, pay_stmt)
  in
  match stmt with
  | Assign _ | Decl _ -> (stmt, [])
  | Seq (s1, s2) ->
    let s1, e1 = convert_call_s cnames fmap s1 in
    let s2, e2 = convert_call_s cnames fmap s2 in
    (Seq (s1, s2), e1 @ e2)
  | Call (ret, Lv (MemberAccess (target, "call", _, _)), args, ethop, gas, loc)
    when Typ.is_address_kind (get_type_exp target) ->
    let eth = ethop |? V (Int Z.zero) in
    let balance_require_stmt = balance_require_stmt eth in
    let balance_stmt = balance_stmt target eth in
    let fkey = Fkey.empty in
    let ether_arg = Option.map (const (Lv pay_amount_memo)) ethop in
    let e =
      {
        Extcall.id = mk_extcall_id ();
        ret = None;
        fkey;
        target;
        args;
        ether = ether_arg;
        gas;
        is_static = false;
        loc;
      }
    in
    let extcall = Stmt.Extcall e in
    (* TODO: inspect calldata and get the correct fn-name *)
    let stmt = Stmt.Seq (balance_stmt, extcall) in
    let exec =
      begin
        match ret with
        | Some (Var v) ->
          Stmt.If
            ( Lv (gen_nondet_selector ()),
              Seq (stmt, Assign (Var v, V (Bool true), Loc.dummy)),
              Assign (Var v, V (Bool false), Loc.dummy),
              Ifinfo.dummy )
        | Some (Tuple (exp_options, _)) ->
          Pp.requiredf (List.length exp_options = 2) "convert_call_s";
          let arbitrary_memory = gen_tmpvar Typ.(EType DBytes) in
          let success_opt = List.hd exp_options in
          let memory_opt = List.hd (List.tl exp_options) in
          let assigning (expopt : exp option) (value : exp) : Stmt.t =
            match expopt with
            | Some (Lv lv) -> Assign (lv, value, Loc.dummy)
            | Some _ -> Pp.failwithf "idk"
            | None -> Skip
          in
          let assigning_success : Stmt.t =
            If
              ( Lv (gen_nondet_selector ()),
                Seq (stmt, assigning success_opt (V (Bool true))),
                assigning success_opt (V (Bool false)),
                Ifinfo.dummy )
          in
          let assigning_memory = assigning memory_opt (Lv arbitrary_memory) in
          Seq (assigning_success, assigning_memory)
        | Some _ ->
          Pp.failwithf "convert_call_s : type of return values of address.call does not match"
        | None -> stmt
      end
    in
    (Seq (balance_require_stmt, exec), [ e ])
  | Call (ret, Lv (MemberAccess (target, instr, vinfo, typ)), args, ether, gas, loc)
    when Typ.is_address_kind (get_type_exp target) && List.mem instr [ "send"; "transfer" ] ->
    assert (no_eth_gas_modifiers stmt);
    assert (List.length args = 1);
    let stmt' =
      Stmt.Call
        (ret, Lv (MemberAccess (target, instr, vinfo, typ)), [ Lv pay_amount_memo ], ether, gas, loc)
    in
    let eth = List.hd args in
    let balance_require_stmt = balance_require_stmt eth in
    let balance_stmt = balance_stmt target eth in
    let balance_stmt = Stmt.Seq (balance_stmt, stmt') in
    begin
      match (instr, ret) with
      | "transfer", _ -> (Stmt.Seq (balance_require_stmt, balance_stmt), [])
      | "send", Some lv ->
        let s =
          Stmt.If
            ( Lv (gen_nondet_selector ()),
              Seq (balance_stmt, Assign (lv, V (Bool true), loc)),
              Assign (lv, V (Bool false), loc),
              Ifinfo.dummy )
        in
        (Stmt.Seq (balance_require_stmt, s), [])
      | "send", None ->
        let s = Stmt.If (Lv (gen_nondet_selector ()), balance_stmt, Skip, Ifinfo.dummy) in
        (Stmt.Seq (balance_require_stmt, s), [])
      | _ -> assert false (* unreachable *)
    end
  | Call _ when FuncMap.is_undef_call fmap stmt -> (stmt, [])
  | Call _ when FuncMap.is_internal_call fmap cnames stmt -> (stmt, []) (* internal call *)
  | Call (ret, e, args, ether, _, loc) when e |> get_target_and_mem |> Option.get |> fst |> is_this
    ->
    (* call to `this` desugared as an internal call *)
    let this = this !main_contract in
    let temp_caller = gen_tmpvar ~loc:loc.line (EType Address) in
    let temp_value = gen_tmpvar ~loc:loc.line (EType (UInt 256)) in
    let memo_prev_caller = Stmt.Assign (temp_caller, Lv msg_sender, loc) in
    let memo_prev_value = Stmt.Assign (temp_value, Lv msg_value, loc) in
    let assign_new_caller = Stmt.Assign (msg_sender, this, loc) in
    let new_value = ether |? V (Int Z.zero) in
    let assign_new_value = Stmt.Assign (msg_value, new_value, loc) in
    let reset_prev_caller = Stmt.Assign (msg_sender, Lv temp_caller, loc) in
    let reset_prev_value = Stmt.Assign (msg_value, Lv temp_value, loc) in
    let _, mem = Option.get (get_target_and_mem e) in
    let intcall_var = Var (mem, mk_vinfo ~typ:(get_type_exp e) ()) in
    let args = List.map (replace_exp (Lv msg_sender) (Lv temp_caller)) args in
    let args = List.map (replace_exp (Lv msg_value) (Lv temp_value)) args in
    let call = Stmt.Call (ret, Lv intcall_var, args, None, None, loc) in
    let stmt =
      Stmt.list_to_seq
        [
          memo_prev_caller;
          memo_prev_value;
          assign_new_caller;
          assign_new_value;
          call;
          reset_prev_caller;
          reset_prev_value;
        ]
    in
    (stmt, [])
  | Call (ret, e, args, ether, gas, loc) ->
    (* object calls *)
    let rcv, mem = Option.get (get_target_and_mem e) in
    let rcv_str = to_string_exp rcv in
    if String.equal rcv_str "super" || List.mem rcv_str cnames then (stmt, [])
    else begin
      let balance_require_stmt = Option.map balance_require_stmt ether |? Stmt.Skip in
      let balance_stmt = Option.map (balance_stmt rcv) ether |? Stmt.Skip in
      let contract_name = Typ.to_contract_name_exn (get_type_exp rcv) in
      let fkey = Fkey.mk contract_name mem (List.map get_type_exp args) in
      match FuncMap.find_opt fkey fmap with
      | Some callee ->
        (* only function calls with the opcode STATICCALL are reentrancy-free, which is 
           supported for view/pure functions when solc >=0.5.0 *)
        let is_static =
          Options.(get_solc_ver ()) >= Solc.Ver.mk 0 5 0 && Func.is_view_pure callee
        in
        (* NOTE: return value can be nondeterministic even if we know the contract src code, 
            due to the inheritance issue *)
        let e =
          {
            Extcall.id = mk_extcall_id ();
            ret;
            fkey;
            target = rcv;
            args;
            ether;
            gas;
            is_static;
            loc;
          }
        in
        let stmt = Stmt.list_to_seq [ balance_require_stmt; balance_stmt; Extcall e ] in
        (stmt, [ e ])
      | None -> (stmt, [])
    end
  | If (e, s1, s2, i) ->
    let s1, e1 = convert_call_s cnames fmap s1 in
    let s2, e2 = convert_call_s cnames fmap s2 in
    (If (e, s1, s2, i), e1 @ e2)
  | While (e, s) ->
    let s, ext = convert_call_s cnames fmap s in
    (While (e, s), ext)
  | Extcall _ | Skip | Break | Continue | Return _ | Revert | Assume _ | Assert _ | Assembly _
  | Placeholder | Label _ ->
    (stmt, [])
  | Unchecked (lst, loc) ->
    let lst = List.map (convert_call_s cnames fmap) lst in
    let lst, ext = List.split lst in
    let ext = List.concat ext in
    (Unchecked (lst, loc), ext)
;;

(**************************************************)
(*** Desugar payables                           ***)
(**************************************************)

(** currently not used *)
let verismart_pay (is_payable : bool) (c : Contract.t) : Func.t =
  let name = if is_payable then "@verismart_pay" else "@verismart_no_pay" in
  let info = mk_finfo ~vis:Vis.Private c in
  let this_balance = this_balance c.name in
  (* msg.value <= maximum send restrict (to consider likely scenarios only) *)
  let msg_value_restrict =
    Stmt.Assume
      (BinOp (LEq, Lv msg_value, V (Int !Options.send_eth), mk_einfo (EType Bool)), Loc.dummy)
  in
  let restrict_msg_value_is_zero =
    Stmt.Assume (BinOp (Eq, Lv msg_value, V (Int Z.zero), mk_einfo (EType Bool)), Loc.dummy)
  in
  let balance_increased =
    Stmt.Assign
      ( this_balance,
        BinOp (Add, Lv this_balance, Lv msg_value, mk_einfo (EType (UInt 256))),
        Loc.dummy )
  in
  let not_trusted target =
    mk_eq (Lv (IndexAccess (Lv trust_map, Some target, EType Bool))) (V (Bool false))
  in
  let invest_increased =
    Stmt.Assign
      ( invest_sum,
        Ite
          ( not_trusted (Lv msg_sender),
            BinOp (Add, Lv invest_sum, Lv msg_value, mk_einfo (EType (UInt 256))),
            Lv invest_sum,
            EType (UInt 256) ),
        Loc.dummy )
  in
  let payable_body = Stmt.Seq (msg_value_restrict, Seq (balance_increased, invest_increased)) in
  let nonpayable_body = restrict_msg_value_is_zero in
  Func.mk ~name ~params:[] ~ret_params:[]
    ~body:(if is_payable then payable_body else nonpayable_body)
    ~info
;;

let insert_verismart_pay (c : Contract.t) : Contract.t =
  let pay = verismart_pay true c in
  let no_pay = verismart_pay false c in
  let update (funcs : Func.t list) = funcs @ [ pay; no_pay ] in
  Field.map Contract.Fields.funcs ~f:update c
;;

let split_trusted_if_payable func (body : Stmt.t) : Stmt.t =
  let split (body : Stmt.t) : Stmt.t =
    let trusted =
      mk_eq (Lv (IndexAccess (Lv trust_map, Some (Lv msg_sender), EType Bool))) (V (Bool true))
    in
    If (trusted, body, body, Ifinfo.dummy)
  in
  if Func.is_payable func then split body else body
;;

let do_all_f cnames (fmap : FuncMap.t) (func : Func.t) =
  let body = func.body in
  let body, extcalls = convert_call_s cnames fmap body in
  let body =
    if !Options.mode = Exploit && !Options.split_payable then split_trusted_if_payable func body
    else body
  in
  let body =
    if Func.is_constructor func then body
    else if not !Chk.reg then body
    else invalid_assign_s cnames fmap body body
  in
  let func = Field.fset Func.Fields.body func body in
  let func = Field.fset Func.Fields.extcall_locations func extcalls in
  func
;;

let do_all_c cnames fmap (c : Contract.t) =
  let c = Field.map Contract.Fields.funcs ~f:(List.map (do_all_f cnames fmap)) c in
  insert_verismart_pay c
;;

let do_all cnames fmap p = List.map (do_all_c cnames fmap) p

let run (p : Pgm.t) : Pgm.t =
  let fmap = FuncMap.mk_fmap p in
  let cnames = Pgm.cnames p in
  let p = do_all cnames fmap p in
  let p = Preprocess.rmskip p in
  p
;;
