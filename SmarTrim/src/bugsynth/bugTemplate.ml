open! Repair
open! Frontend
open Frontend.Lang
open Repair.Patch
open Vocab

type name = string
and input_param = Var.t
and ret_param = Var.t

type line = int

(*************************)
(*** abstract template ***)
(*************************)

(* Q. Why differentiate 'gvars' and 'lvars'? *)
(* A. To address imprecision of existing word embedding *)
(* E.g. sim ('owner','account') > sim ('to','account') *)

type at = name * gvars_seed * aa list
and gvars_seed = Var.t BatSet.t

and aa = (* abstract atom *)
  | A_ReplaceExp of Fkey.t * exp * exp
  | A_ReplaceAssign of Fkey.t * (lv * exp) * (lv * exp)
  | A_RemoveStmt of Fkey.t * Stmt.t
  | A_ApplyUnchecked of Fkey.t * Stmt.t (* only atomic stmt *)
  | A_RemoveModifier of Fkey.t * name
  | A_Remove_IO_Guard
  | A_Remove_ModCalls of name
  | A_Func of (name * input_param list * ret_param list * name list * Stmt.t * payable)

  | A_Decl of Fsig.t * Var.t * exp
  | A_Assert of Fsig.t * exp

and payable = bool

let eq_aa aa1 aa2 =
  match aa1, aa2 with
  | A_ReplaceExp (_,e1,e2), A_ReplaceExp (_,e1',e2') ->
    to_string_exp ~report:true e1 = to_string_exp ~report:true e1'
    && to_string_exp ~report:true e2 = to_string_exp ~report:true e2'
  | A_RemoveStmt (_,s), A_RemoveStmt (_,s') -> to_string_stmt ~report:true s = to_string_stmt ~report:true s'
  | A_ApplyUnchecked (_,s), A_ApplyUnchecked (_,s') -> to_string_stmt ~report:true s = to_string_stmt ~report:true s'
  | A_RemoveModifier (_,mname), A_RemoveModifier (_,mname') -> mname = mname'
  | A_Remove_IO_Guard, A_Remove_IO_Guard -> true
  | A_Remove_ModCalls x, A_Remove_ModCalls x' -> x = x'

  | A_Decl (_fsig,var,exp), A_Decl (_fsig',var',exp') -> var = var' && to_string_exp ~report:true exp = to_string_exp ~report:true exp'
  | A_Assert (_,exp), A_Assert (_,exp') -> to_string_exp ~report:true exp = to_string_exp ~report:true exp'
  | A_Func (name,input,ret,mods,s,b), A_Func (name',input',ret',mods',s',b') ->
    name=name' && input=input' && ret=ret' && List.length mods = List.length mods' && List.for_all2 (=) mods mods'
    && to_string_stmt ~report:true s = to_string_stmt ~report:true s' && b = b'
  | _ -> false

let eq_at : at -> at -> bool
= fun (_,_,lst1) (_,_,lst2) ->
  if List.length lst1 <> List.length lst2 then false
  else List.for_all2 eq_aa lst1 lst2 (* XXX : consider gvars? *)

let to_string_aa : aa -> string
= fun aa ->
  match aa with
  | A_ReplaceExp (_,e1,e2) -> "A_ReplaceExp (" ^ to_string_exp e1 ^ " -> " ^ to_string_exp e2 ^ ")"
  | A_ReplaceAssign (_,(lv1,e1),(lv2,e2)) ->
    "A_ReplaceAssign (" ^
    to_string_stmt (Assign (lv1,e1,Loc.dummy)) ^ " -> "
    ^ to_string_stmt (Assign (lv2,e2,Loc.dummy)) ^ ")"
  | A_RemoveStmt (_,stmt) -> "A_RemoveStmt (" ^ to_string_stmt stmt ^ ")"
  | A_RemoveModifier (_,name) -> "A_RemoveModifier (" ^ name ^ ")"
  | A_ApplyUnchecked (_,stmt) -> "A_ApplyUnchecked (" ^ to_string_stmt stmt ^ ")"
  | A_Remove_IO_Guard -> "A_Remove_IO_Guard"
  | A_Remove_ModCalls name -> "A_Remove_ModCalls_" ^ name

  | A_Decl (fsig, var, e) ->
    "A_Decl (" ^ to_string_fsig fsig ^ ", "
    ^ Typ.to_string (snd var) ^ " " ^ Var.origin_name (fst var)
    ^ " = " ^ to_string_exp e ^ ")"
  | A_Assert (fsig, e) -> "A_Assert (" ^ to_string_fsig fsig ^ ", " ^ to_string_stmt (Assert (e,"",Loc.dummy)) ^ ")"
  | A_Func (name,_,_,_,_,_) -> "A_Func (" ^ name ^ ")"

let to_string_at : at -> string
= fun (name,_,lst) ->
  if List.length lst = 0 then "[]"
  else
  "=== " ^ name ^ " ===\n" ^
  List.fold_left (fun acc aa ->
    if acc = "" then to_string_aa aa
    else acc ^ ",\n" ^ to_string_aa aa
  ) "" lst

(*************************)
(*** concrete template ***)
(*************************)

type ct = name * ca list

(* concrete atom *)
and ca =
  | C_ReplaceExp of Loc.t * exp * exp
  | C_ReplaceAssign of Loc.t * (lv * exp) * (lv * exp)
  | C_RemoveStmt of Loc.t * Stmt.t
  | C_RemoveModifier of Loc.t * Mod_call.t
  | C_ApplyUnchecked of Loc.t * Loc.t (* (starting loc, ending loc) *)
  | C_Decl of Loc.t * Var.t * exp
  | C_Assert of Loc.t * exp
  | C_Func of int * name * input_param list * ret_param list * string list * Stmt.t * bool

let eq_ca ca1 ca2 =
  match ca1,ca2 with
  | C_ReplaceExp (loc,e1,e2), C_ReplaceExp (loc',e1',e2') ->
    loc.line = loc'.line && to_string_exp e1 = to_string_exp e1' && to_string_exp e2 = to_string_exp e2'
  | C_ReplaceAssign (loc,(lv1,e1),(lv2,e2)), C_ReplaceAssign (loc',(lv1',e1'),(lv2',e2')) ->
    loc.line = loc'.line
    && to_string_lv lv1 = to_string_lv lv1'
    && to_string_exp e1 = to_string_exp e1'
    && to_string_lv lv2 = to_string_lv lv2'
    && to_string_exp e2 = to_string_exp e2'
  | C_RemoveStmt (loc,s), C_RemoveStmt (loc',s') ->
    loc.line = loc'.line && to_string_stmt s = to_string_stmt s'
  | C_RemoveModifier (loc,mcall), C_RemoveModifier (loc',mcall') ->
    loc.line = loc'.line && to_string_mod mcall = to_string_mod mcall'
  | C_ApplyUnchecked (loc1,loc2), C_ApplyUnchecked (loc1',loc2') ->
    loc1.line = loc1'.line && loc2.line = loc2'.line
  | C_Decl (loc,v,e), C_Decl (loc',v',e') ->
    loc.line = loc'.line && v=v' && to_string_exp e = to_string_exp e'
  | C_Assert (loc,e), C_Assert (loc',e') ->
    loc.line = loc'.line && to_string_exp e = to_string_exp e'
  | C_Func (_,name,_,_,_,_,_), C_Func (_,name',_,_,_,_,_) -> name = name'
  | _ -> false

let to_string_ca ca =
  match ca with
  | C_RemoveStmt (loc,s) ->
    "C_RemoveStmt (" ^ string_of_int loc.line ^ ", " ^ to_string_stmt ~report:true s ^ ")"
  | C_RemoveModifier (loc,mcall) ->
    "C_RemoveModifier (" ^ string_of_int loc.line ^ ", " ^ to_string_mod mcall ^ ")"
  | C_ReplaceExp (loc, e1, e2) ->
    "C_ReplaceExp (" ^ string_of_int loc.line ^ ", " ^
    to_string_exp ~report:true e1 ^ " => " ^
    to_string_exp ~report:true e2 ^ ")"
  | C_ReplaceAssign (loc, (lv1,e1), (lv2,e2)) ->
    "C_ReplaceAssign (" ^ string_of_int loc.line ^ ", " ^
    to_string_stmt ~report:true (Assign (lv1,e1,Loc.dummy)) ^ " => " ^
    to_string_stmt ~report:true (Assign (lv2,e2,Loc.dummy)) ^ ")"
  | C_ApplyUnchecked (sloc,eloc) ->
    "C_ApplyUnchecked (" ^
    (if sloc.line = eloc.line then string_of_int sloc.line
     else string_of_int sloc.line ^ "-" ^ string_of_int eloc.line)
    ^ ")"
  | C_Decl (loc,var,e) ->
    "C_Decl (" ^ string_of_int loc.line ^ ", "
    ^ Typ.to_string (snd var) ^ " " ^ Var.origin_name (fst var) ^ " = "  ^ to_string_exp ~report:true e ^ ")"
  | C_Assert (loc,e) ->
    "C_Assert (" ^ string_of_int loc.line ^ ", " ^ to_string_exp ~report:true e ^ ")"
  | C_Func (line,_name,_params,_ret_params,_mcalls,_stmt,_payable) ->
    "C_Func (" ^ string_of_int line ^ ")"

let to_string_ct : ct -> string
= fun (name,lst) ->
  if List.length lst = 0 then "[]"
  else
  "=== " ^ name ^ " ===\n" ^
  List.fold_left (fun acc ca ->
    if acc = "" then to_string_ca ca
    else acc ^ ",\n" ^ to_string_ca ca
  ) "" lst

(***********************************)
(***** Apply Concrete Template *****)
(***********************************)

let sep2 = "  "

let rewrite_eth_rcv solv rcv =
  let typ = get_type_exp rcv in
  if Typ.is_address typ && solv >= Solc.Ver.mk 0 6 0 then
    Cast (EType AddressPayable, rcv)
  else if Typ.is_address_payable typ && solv < Solc.Ver.mk 0 6 0 then
    (match rcv with
     | Cast (EType AddressPayable, rcv') -> rcv'
     | _ -> rcv)
  else rcv

let rec to_string_stmt solv indent (stmt : Stmt.t) =
  match stmt with
  | Seq (Skip,s2) -> to_string_stmt solv indent s2
  | Seq (s1,s2) ->
    to_string_stmt solv indent s1 ^ "\n"
    ^ to_string_stmt solv indent s2

  | Call (lvop,Lv (Var (fname,info)), [arg], ethop,gasop,loc)
    when List.mem fname ["selfdestruct";"suicide"] ->
    let arg' = rewrite_eth_rcv solv arg in
    let stmt' = Stmt.Call (lvop,Lv (Var (fname,info)), [arg'], ethop,gasop,loc) in
    indent ^ Lang.to_string_stmt ~solv ~report:true stmt'

  | Call (lvop,Lv (MemberAccess (e,fname,info,typ)),args,ethop,gasop,loc)
    when List.mem fname ["transfer"; "send"; "call"] ->
    let e' = rewrite_eth_rcv solv e in
    let stmt' = Stmt.Call (lvop,Lv (MemberAccess (e',fname,info,typ)),args,ethop,gasop,loc) in
    indent ^ Lang.to_string_stmt ~solv ~report:true stmt'

  | Assign (Tuple ([Some (Lv (Var (v,vinfo))); None], _),
            CallTemp (Lv (MemberAccess (e,fname,info,typ)), args, ethop, gasop, _),
            _loc)
    when fname = "call" ->
    let e' = rewrite_eth_rcv solv e in
    indent ^ "(" ^ Typ.to_string vinfo.vtyp ^ " " ^ Lang.to_string_lv ~solv ~report:true (Var (v,vinfo)) ^ ", )" ^ " = " ^
    Lang.to_string_exp ~solv ~report:true (Lv (MemberAccess (e',fname,info,typ))) ^
    Lang.to_string_ethop_gasop ~solv ~report:true ethop gasop ^
    string_of_list ~first:"(" ~last:")" ~sep:", " (Lang.to_string_exp ~solv ~report:true) args ^ ";"

  | If (e,Skip,Revert,_) ->
    indent ^ "require (" ^ Lang.to_string_exp ~solv ~report:true e ^ ");"

  | If (e,s1,s2,_) ->
    indent ^ "if (" ^ Lang.to_string_exp ~solv ~report:true e ^ ") {\n"
    ^ to_string_stmt solv (indent ^ sep2) s1
    ^ indent ^ "}\n"
    ^ indent ^ "else {\n"
    ^ to_string_stmt solv (indent ^ sep2) s2
    ^ indent ^ "}"
  | While (e,s) ->
    indent ^ "while (" ^ Lang.to_string_exp ~solv ~report:true e ^ ") {\n"
    ^ to_string_stmt solv (indent ^ sep2) s
    ^ indent ^ "}"
  | Skip -> ""
  | Unchecked (lst, _) ->
    indent ^ "unchecked {\n"
    ^ (Vocab.string_of_list ~first:"" ~last:"\n" ~sep:"\n" (to_string_stmt solv (indent ^ sep2)) lst)
    ^ indent ^ "}"
  | _ -> indent ^ Lang.to_string_stmt ~solv ~report:true stmt

let apply' : Solc.Ver.t -> ca -> string list -> string list
= fun solv ca lines ->
  match ca with
  | C_RemoveModifier (loc,mcall) ->
    let regex_mcall ({ id = mname; args; _ } : Mod_call.t) : string list =
      let rspace = Patch.rspace in
      if List.length args = 0 then [mname ^ rspace ^ "(" ^ rspace ^ ")"; mname]
      else
        let args_lst = args |> List.map Patch.regex_exp |> BatList.n_cartesian_product in
        List.fold_left (fun acc args' ->
          acc @ [Vocab.string_of_list ~first:"(" ~last:")" ~sep:("," ^ rspace) Vocab.id args']
        ) [] args_lst
    in
    let regex_lst = regex_mcall mcall in
    BatList.modify_at (loc.line-1) (fun x ->
      Patch.replace x regex_lst ("/* <Inject> RemoveModifier : " ^ (BatList.last regex_lst) ^ " */ ")
    ) lines

  | C_ReplaceExp (loc,e1,e2) ->
    let regex = Patch.regex_exp e1 in
    let org = Lang.to_string_exp ~solv ~report:true e1 in
    let rep = Lang.to_string_exp ~solv ~report:true e2 in
    BatList.modify_at (loc.line-1) (fun x ->
      Patch.replace x regex rep ^ " /* <Inject> ReplaceExp : " ^ org ^ " */"
    ) lines

  | C_ReplaceAssign (loc,(lv1,e1),(lv2,e2)) ->
    let regex_assign (lv,e) =
      let lst1, lst2 = Patch.regex_lv lv, Patch.regex_exp e in
      let product = BatList.cartesian_product lst1 lst2 in
      List.map (fun (r1,r2) -> r1 ^ rspace ^ "=" ^ rspace ^ r2 ^ rspace ^ ";") product
    in
    let regex = regex_assign (lv1,e1) in
    let rep = Lang.to_string_stmt ~report:true (Assign (lv2,e2,Loc.dummy)) in
    let (org_core,rep_core) =
      if Lang.to_string_lv ~solv ~report:true lv1 = Lang.to_string_lv ~solv ~report:true lv2 then
        (Lang.to_string_exp ~solv ~report:true e1, Lang.to_string_exp ~solv ~report:true e2)
      else if Lang.to_string_exp ~solv ~report:true e1 = Lang.to_string_exp ~solv ~report:true e2 then
        (Lang.to_string_lv ~solv ~report:true lv1, Lang.to_string_lv ~solv ~report:true lv2)
      else
        (Lang.to_string_stmt ~solv ~report:true (Assign (lv1,e1,Loc.dummy)),
         Lang.to_string_stmt ~solv ~report:true (Assign (lv2,e2,Loc.dummy)))
    in
    BatList.modify_at (loc.line-1) (fun x ->
      Patch.replace x regex rep ^ " /* <Inject> ReplaceAssign : " ^ org_core ^ " => " ^ rep_core ^ " */"
    ) lines

  | C_ApplyUnchecked (sloc,_eloc) when sloc.line <> sloc.finish_line -> (* function block condition *)
    let len = sloc.finish_line - sloc.line + 1 in
    lines
    |> BatList.modify_at (sloc.line-1) (fun x ->
         let by = "{" ^ " " ^ "unchecked { /* <Inject> Apply Unchecked Block __" ^ string_of_int len ^ "__ */" in
         snd (BatString.replace ~str:x ~sub:"{" ~by:by))
    |> BatList.modify_at (sloc.finish_line - 1) (fun x ->
         snd (BatString.replace ~str:x ~sub:"}" ~by:"}}"))

  | C_ApplyUnchecked (sloc,eloc) when sloc.line = eloc.line -> (* single statement *)
    BatList.modify_at (sloc.line-1) (fun x ->
      space_of x ^ "unchecked { /* <Inject> Apply Unchecked Block __3__ */\n"
      ^ "  " ^ x ^ "\n"
      ^ space_of x ^ "}"
    ) lines

  | C_ApplyUnchecked (sloc,eloc) when sloc.line <> eloc.line -> (* multiple statements *)
    let len = eloc.line - sloc.line + 1 in
    lines
    |> BatList.modify_at (sloc.line-1) (fun x ->
         space_of x ^ "unchecked { /* <Inject> Apply Unchecked Block __" ^ string_of_int len ^ "__ */\n"
         ^ "  " ^ x)
    |> BatList.modify_at (eloc.line-1) (fun x ->
         "  " ^ x ^ "\n"
         ^ space_of x ^ "}")

  | C_ApplyUnchecked _ -> assert false

  | C_RemoveStmt (loc,_str) ->
    BatList.modify_at (loc.line-1) (fun x ->
      space_of x ^ " /* <Inject> RemoveStmt : " ^ strip_space x ^ "*/ "
    ) lines

  (* TODO : check needs *)
  | C_Decl (loc,var,exp) ->
    let y = BatList.at lines loc.line in (* refer to next line *)
    let to_string (var,exp) = Typ.to_string (snd var) ^ " " ^ Var.origin_name (fst var) ^ " = " ^ to_string_exp ~solv ~report:true exp ^ ";" in
    let str = to_string (var,exp) in
    if not (space_of y = "") then
      BatList.modify_at loc.line (fun x ->
        space_of x ^ str ^ " /* <Inject> InsertDecl */" ^ "\n" ^ x
      ) lines
    else (* XXX *)
      BatList.modify_at (loc.line-1) (fun x ->
        x ^ "\n" ^ space_of x ^ "  " ^ str ^ " /* <Inject> InsertDecl */"
      ) lines

  | C_Assert (loc,e) ->
    let y1, y2 = BatList.at lines (loc.finish_line - 2), BatList.at lines (loc.finish_line - 1) in
    let to_string exp = "assert(" ^ to_string_exp ~solv ~report:true exp ^ ");" in
    let str = to_string e in
    if not (space_of y1 = "") && not (BatString.contains y2 ';') then
      BatList.modify_at (loc.finish_line - 2) (fun x ->
        x ^ "\n" ^ space_of x ^ str ^ " /* <Inject> InsertAsert */"
      ) lines
    else (* XXX *)
      BatList.modify_at (loc.finish_line - 1) (fun x ->
        space_of x ^ "  " ^ str ^ " /* <Inject> InsertAssert */" ^ "\n" ^ x
      ) lines

  | C_Func (line, name, input, ret, mcalls, stmt, payable) ->
    let mcalls_str = if List.length mcalls = 0 then "" else Vocab.string_of_list ~first:" " ~sep:" " ~last:"" Vocab.id mcalls in
    let start_str =
      let var_to_str x = Typ.to_string (snd x) ^ " " ^ Var.origin_name (fst x) in
      let input_str = Vocab.string_of_list ~first:"(" ~sep:", " ~last:")" var_to_str input in
      let ret_str = if List.length ret = 0 then "" else Vocab.string_of_list ~first:" returns (" ~sep:", " ~last:")" var_to_str ret in
      "function " ^ name ^ " " ^ input_str ^ " " ^ "public" ^ mcalls_str ^ (if payable then " payable " else "") ^ ret_str
    in
    BatList.modify_at (line - 1) (fun x ->
      let indent = space_of x in
      let body_str = to_string_stmt solv (indent ^ sep2) stmt in
      let body_len = BatString.count_char body_str '\n' + 1 in
      let total_len = 2 + body_len in
      x ^ "\n\n"
      ^ indent ^ "/* <Inject> InsertFunc __" ^ string_of_int total_len ^ "__ */" ^ "\n"
      ^ indent ^ start_str ^ " {\n"
      ^ body_str ^ "\n"
      ^ indent ^ "}\n"
    ) lines

let collect_ulines : ct -> line BatSet.t
= fun (_name,lst) ->
  List.fold_left (fun acc ca ->
    match ca with
    | C_ApplyUnchecked (sloc,eloc) ->
      let _ = assert (sloc.line = eloc.line) in
      BatSet.add sloc.line acc
    | _ -> acc
  ) BatSet.empty lst

let rec find_adj_min : line -> line BatSet.t -> line
= fun line set ->
  if BatSet.mem line set then find_adj_min (line-1) set
  else line+1

let rec find_adj_max : line -> line BatSet.t -> line
= fun line set ->
  if BatSet.mem line set then find_adj_max (line+1) set
  else line-1

let find_adj_min_max : line -> line BatSet.t -> line * line
= fun line set -> (find_adj_min line set, find_adj_max line set)

let rec simplify' : line BatSet.t -> ca list -> ca list
= fun lines lst ->
  match lst with
  | [] -> []
  | (C_ApplyUnchecked (sloc,eloc))::tl ->
    let _ = assert (sloc.line = eloc.line) in
    let (min,max) = find_adj_min_max sloc.line lines in
    let (sloc,eloc) = ({sloc with line = min}, {eloc with line = max}) in
    (C_ApplyUnchecked (sloc,eloc))::(simplify' lines tl)
  | hd::tl -> hd::(simplify' lines tl)

let simplify : line BatSet.t -> ct -> ct
= fun lines (name,lst) -> (name, simplify' lines lst)

let dedup_unchecked : ct -> ct
= fun (name,lst) ->
  List.fold_left (fun acc ca ->
    if List.length acc = 0 then [ca]
    else if List.exists (eq_ca ca) acc then acc
    else acc @ [ca]
  ) [] lst
  |> (fun res -> (name,res))

let simplify_unchecked : ct -> ct
= fun ct ->
  ct
  |> simplify (collect_ulines ct)
  |> dedup_unchecked

(* preprocess ct before applying it *)
let preprocess_ct : ct -> ct
= fun ct -> ct |> simplify_unchecked

let apply : Solc.Ver.t -> string -> ct -> string list -> (bool * string list)
= fun solv _fid ct flines ->
  (* let _ = print_endline fid in *)
  try
    let (_name,lst) = preprocess_ct ct in
    (true, Vocab.list_fold (apply' solv) lst flines)
  with _ -> (false, [])
