(** Usage: [open Vocab.Shadow_batteries], and the compiler will force not to use [Bat*] modules and
    enforce [open Vocab.Batteries]. Useful for keeping code clean. *)

module Void = struct end
module BatBig_int = Void
module BatDeque = Void
module BatDynArray = Void
module BatEnum = Void
module BatInt = Void
module BatList = Void
module BatMap = Void
module BatOption = Void
module BatRandom = Void
module BatResult = Void
module BatSet = Void
module BatString = Void
