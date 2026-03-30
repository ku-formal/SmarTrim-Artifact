(** Intermediate Expressions. This module has two purposes:

    1. This module will be a frontend for different solvers.

    2. This module will be a replacement to the {!Formula.Vlang}, which is not perfectly well-typed
    and may fail when distinguishing signed/unsigned expressions. Also, this module will use GADT to
    catch the type error on EXPR in compile time.

    Currently in developing and unused anywhere.

    Written as of 2025. *)

open! Vocab.Batteries

module Unop = struct
  type t = Not  (** logical not *) | Neg  (** negative integer sign *)
  [@@deriving show { with_path = false }]
end

module Binop = struct
  type t =
    | And
    | Or
    | Xor
    | Eq
    | Lt
    | Leq
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Pow
    | Shl
    | Shr
    | Concat  (** bitvector concatenation*)
  [@@deriving show { with_path = false }]
end
