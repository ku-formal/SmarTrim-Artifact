[@@@warning "-32-37"]

(** just an example of GADTs, doesn't used anywhere. *)

type[@warning "-38"] _ value = Int : int -> int value | Bool : bool -> bool value

type _ expr =
  | Value : 'a value -> 'a expr
  | Eq : int expr * int expr -> bool expr
  | Plus : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr

let eval_value : type a. a value -> a = function Int x -> x | Bool x -> x

let rec eval : type a. a expr -> a = function
  | Value v -> eval_value v
  | If (c, t, e) -> if eval c then eval t else eval e
  | Eq (x, y) -> eval x = eval y
  | Plus (x, y) -> eval x + eval y
;;

type zero = Z
type _ succ = S : 'n -> 'n succ
type three = zero succ succ succ

type (_, _, _) add =
  | AddZ : (zero, 'n, 'n) add
  | AddS : ('m, 'n, 'o) add -> ('m succ, 'n, 'o succ) add

let x : (zero, three, three) add = AddZ
(* we can produce (zero, three, three) add type object, but cannot (zero, three, zero) object *)
