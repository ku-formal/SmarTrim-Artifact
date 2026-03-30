(** A replacement of {!Options}, with functional style coding (no [ref]s). *)

module Common = struct
  type t = {
    input : string;
    main : string option;
    report : string option;
        (** Some dirpath: generate reports to dirpath ; None: do not generate reports *)
    solv : Solc.Ver.t;
    delayed_safety : bool;
    inline_depth : int;
    debug : string;
  }
  [@@deriving show]
end

module Exploit = struct
  type t = {
    timeout : int;
        (** keep {!Verify.timeout} and {!Exploit.timeout} different, as someday we may need the
            integrated program of Verify & Exploit *)
    depth : int;
    ngram : int;
    lm_path : string option;
    validate : bool;
    init_eth : int;
    send_eth : int;
    pruning : bool;
    candidates : int list;
  }
  [@@deriving show]
end

module Subconfigs = struct
  type 'a t = { c : Common.t; s : 'a } [@@deriving show]
end

type t =
  | Verify of unit
  | Exploit of Exploit.t Subconfigs.t
  | Repair of unit
  | Bugsynth of unit
  | Il of unit
  | Cfg of unit
  | Solc of unit
[@@deriving show]
