type t' = 
  | V of Z.t
  | PInf 
  | NInf

type t = Itv of t' * t' | Bot  

let top = Itv (NInf, PInf)
let bot = Bot

let zero = Z.zero
let is_pos n = Z.(gt n zero)
let is_zero n = Z.(equal n zero)

let upper_int itv =
  match itv with
  | Itv (_,V n) -> n
  | _ -> failwith "upper_int"

let lower_int itv =
  match itv with
  | Itv (V n,_) -> n
  | _ -> failwith "lower_int"

let to_string' : t' -> string
= fun v -> 
  match v with
  | V n -> Z.to_string n
  | PInf -> "+oo"
  | NInf -> "-oo"

let to_string : t -> string
= fun itv ->
  match itv with
  | Itv (l,u) -> "[" ^ (to_string' l) ^ ", " ^ (to_string' u) ^"]"
  | Bot -> "bot"

let le' : t' -> t' -> bool
= fun v1 v2 ->
  match v1,v2 with
  | NInf,_ -> true
  | _,PInf -> true
  | V n1,V n2 -> Z.leq n1 n2
  | _,_ -> false

let eq' : t' -> t' -> bool
= fun v1 v2 ->
  match v1,v2 with
  | NInf,NInf 
  | PInf,PInf -> true
  | V n1,V n2 -> Z.equal n1 n2
  | _,_ -> false

let lt' : t' -> t' -> bool
= fun v1 v2 -> le' v1 v2 && not (eq' v1 v2)

let gt' : t' -> t' -> bool
= fun v1 v2 -> not (le' v1 v2) (* check *) 

let ge' : t' -> t' -> bool
= fun v1 v2 -> not (lt' v1 v2) 

let min' : t' -> t' -> t' 
= fun v1 v2 -> if le' v1 v2 then v1 else v2

let max' : t' -> t' -> t'
= fun v1 v2 -> if le' v1 v2 then v2 else v1 

let plus' : t' -> t' -> t' 
= fun v1 v2 ->
  match v1,v2 with
  | V n1,V n2 -> V (Z.add n1 n2)
  | PInf,NInf -> failwith "itv.ml : plus'"
  | NInf,PInf -> failwith "itv.ml : plus'"
  | PInf,_ -> PInf
  | NInf,_ -> NInf
  | _,PInf -> PInf
  | _,NInf -> NInf

let minus' : t' -> t' -> t'
= fun v1 v2 ->
  match v1,v2 with
  | V n1,V n2 -> V (Z.sub n1 n2)
  | PInf,PInf -> failwith "itv.ml : minus'"
  | NInf,NInf -> failwith "itv.ml : minus'"
  | PInf,_ -> PInf
  | NInf,_ -> NInf
  | _,PInf -> NInf
  | _,NInf -> PInf 

let times' : t' -> t' -> t'
= fun v1 v2 ->
  match v1,v2 with 
  | V n1,V n2 -> V (Z.mul n1 n2)
  | PInf,PInf
  | NInf,NInf -> PInf
  | PInf,NInf
  | NInf,PInf -> NInf
  | PInf,V n
  | V n,PInf ->
    if Z.gt n zero then PInf else 
    if Z.lt n zero then NInf
    else (V zero)
  | NInf,V n
  | V n,NInf ->
    if Z.gt n zero then NInf else 
    if Z.lt n zero then PInf
    else (V zero)

let divide' : t' -> t' -> t'
= fun v1 v2 ->
  match v1,v2 with
  | V n1, V n2 ->
    if Z.equal n2 zero then failwith "itv.ml : divide'1"
    else V (Z.div n1 n2)
  | PInf,PInf
  | NInf,NInf -> PInf
  | NInf,PInf
  | PInf,NInf -> NInf
  | NInf,V n ->
    if Z.lt n zero then PInf else (* n<0 *)
    if Z.gt n zero then NInf      (* n>0 *)
    else failwith "itv.ml : divide'2"
  | PInf,V n ->
    if Z.lt n zero then NInf else (* n<0 *)
    if Z.gt n zero then PInf      (* n>0 *)
    else failwith "itv.ml : divide'3"
  | V _,PInf
  | V _,NInf -> V zero

let modulo' : t' -> t' -> t'
= fun v1 v2 ->
  match v1,v2 with
  | V n1, V n2 ->
    if Z.equal n2 zero then raise (Failure "itv.ml : divide'2")
    else V (Z.rem n1 n2) 
  | PInf,PInf 
  | PInf,NInf -> PInf
  | NInf,PInf
  | NInf,NInf -> NInf
  | NInf,V n ->
    if Z.lt n zero then V (Z.succ n) else              (* n<0 => n+1 *)
    if Z.gt n zero then V (Z.succ (Z.neg n))  (* n>0 => -n+1 *)
    else raise (Failure "itv.ml : modulo'2")
  | PInf,V n ->
    if Z.lt n zero then V (Z.neg (Z.succ n)) else  (* n<0 => -(n+1) *)
    if Z.gt n zero then V (Z.pred n)                        (* n>0 => (n-1) *)
    else raise (Failure "itv.ml : modulo'3")
  | V n,PInf
  | V n,NInf -> V n

let power' : t' -> t' -> t'
= fun v1 v2 ->
  match v1,v2 with
  | V n1, V n2 -> V (Frontend.Lang.zpow n1 n2)
  | _ -> raise (Failure "power'")

let lower_widen' : t' -> t' -> t'
= fun v1 v2 ->
  if lt' v2 v1 then NInf 
  else v1
  
let upper_widen' : t' -> t' -> t'
= fun v1 v2 ->
  if gt' v2 v1 then PInf
  else v1

let is_const : t -> bool
= fun itv ->
  match itv with
  | Itv (V n1,V n2) when Z.equal n1 n2 -> true
  | _ -> false

let const_of : t -> Z.t
= fun itv ->
  match itv with
  | Itv (V n1,V _) when is_const itv -> n1
  | _ -> raise (Failure "extract_const")

let is_top : t -> bool
= fun itv -> itv = top

let is_bot : t -> bool
= fun itv ->
  match itv with
  | Bot -> true
  | Itv (l,u) -> l = PInf || u = NInf || not (le' l u)

let le : t -> t -> bool
= fun itv1 itv2 ->
  if is_bot itv1 then true else
  if is_bot itv2 then false 
  else
    match itv1,itv2 with
    | Itv (l1,u1),Itv (l2,u2) -> le' l2 l1 && le' u1 u2
    | _ -> raise (Failure "itv.ml : le")

let eq : t -> t -> bool
= fun itv1 itv2 ->
  if is_bot itv1 && is_bot itv2 then true else
  if is_bot itv1 || is_bot itv2 then false
  else
    match itv1,itv2 with
    | Itv (l1,u1),Itv (l2,u2) -> eq' l1 l2 && eq' u1 u2
    | _ -> raise (Failure "itv.ml : eq")

let plus : t -> t -> t
= fun itv1 itv2 ->
  if is_bot itv1 || is_bot itv2 then Bot
  else
    match itv1,itv2 with
    | Itv (l1,u1),Itv (l2,u2) -> Itv (plus' l1 l2, plus' u1 u2) 
    | _ -> raise (Failure "itv.ml : plus") 

let minus : t -> t -> t
= fun itv1 itv2 ->
  if is_bot itv1 || is_bot itv2 then Bot
  else
    match itv1,itv2 with
    | Itv (l1,u1),Itv (l2,u2) -> Itv (minus' l1 u2, minus' u1 l2)
    | _ -> raise (Failure "itv.ml : minus")

let times : t -> t -> t
= fun itv1 itv2 ->
  if is_bot itv1 || is_bot itv2 then Bot
  else
    match itv1,itv2 with
    | Itv (l1,u1),Itv (l2,u2) -> 
      let x1 = times' l1 l2 in
      let x2 = times' l1 u2 in
      let x3 = times' u1 l2 in
      let x4 = times' u1 u2 in 
      let l = min' (min' x1 x2) (min' x3 x4) in
      let u = max' (max' x1 x2) (max' x3 x4) in
      Itv (l,u)
    | _ -> raise (Failure "itv.ml : times")

let divide : t -> t -> t
= fun itv1 itv2 -> (* itv1/itv2 *)
  if is_bot itv1 || is_bot itv2 then bot else
  if eq (Itv (V zero,V zero)) itv2 then top else
  if le (Itv (V zero,V zero)) itv2 then top else
    match itv1,itv2 with
    | Itv (l1,u1),Itv (l2,u2) ->
      let x1 = divide' l1 l2 in
      let x2 = divide' l1 u2 in
      let x3 = divide' u1 l2 in
      let x4 = divide' u1 u2 in
      let l = min' (min' x1 x2) (min' x3 x4) in
      let u = max' (max' x1 x2) (max' x3 x4) in
      Itv (l,u)
    | _ -> raise (Failure "itv.ml : divide")

let modulo : t -> t -> t
= fun itv1 itv2 -> (* itv1 mod itv2 *)
  if is_bot itv1 || is_bot itv2 then bot else
  if eq (Itv (V zero,V zero)) itv2 then top else
  if le (Itv (V zero,V zero)) itv2 then top else
    match itv1,itv2 with
    | Itv (l1,u1),Itv (l2,u2) ->
      let x1 = modulo' l1 l2 in
      let x2 = modulo' l1 u2 in
      let x3 = modulo' u1 l2 in
      let x4 = modulo' u1 u2 in
      let l = min' (min' x1 x2) (min' x3 x4) in
      let u = max' (max' x1 x2) (max' x3 x4) in
      Itv (l,u)
    | _ -> raise (Failure "itv.ml : modulo")

let power : t -> t -> t 
= fun itv1 itv2 ->
  if is_bot itv1 || is_bot itv2 then Bot
  else
    match itv1,itv2 with
    | Itv (V l1,V u1),Itv (V l2,V u2)
      when is_pos l1 && is_pos u1 && (is_pos l2 || is_zero l2) && (is_pos u2 || is_zero l2) ->
      Itv (power' (V l1) (V l2), power' (V u1) (V u2))
    | Itv _,Itv _ -> top
    | _ -> raise (Failure "itv.ml : times")

let join : t -> t -> t
= fun itv1 itv2 ->
  if le itv1 itv2 then itv2 else
  if le itv2 itv1 then itv1
  else
    match itv1,itv2 with
    | Itv (l1,u1),Itv (l2,u2) -> Itv (min' l1 l2, max' u1 u2)  
    | _ -> raise (Failure "itv.ml : join")

let meet : t -> t -> t
= fun itv1 itv2 ->
  if le itv1 itv2 then itv1 else (* bot related op is included in le *) 
  if le itv2 itv1 then itv2
  else
    match itv1,itv2 with
    | Itv (l1,u1),Itv (l2,u2) -> Itv (max' l1 l2, min' u1 u2)  
    | _ -> raise (Failure "itv.ml : meet") 

let widen : t -> t -> t
= fun itv1 itv2 ->
  if is_bot itv1 then itv2 else
  if is_bot itv2 then itv1
  else
    match itv1,itv2 with
    | Itv (l1,u1),Itv (l2,u2) -> 
      Itv (lower_widen' l1 l2, upper_widen' u1 u2) 
    | _ -> raise (Failure "itv.ml : widen") 
