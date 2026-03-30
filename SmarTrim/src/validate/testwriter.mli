(** [Validate.Testwriter]. Automatically synthesize attack scenarios and contracts. *)

(** genearte a testfile, which will be a content of Counter.t.sol.

    Test contents are divided into two sections, the {b set} section and the {b test} section. These
    names just follows the practice of Foundry document, and actually lots of test will be happen in
    the {b set} section. *)
val get_testfile : Concrete.Tseq.t -> string

(** genearte an attack contract, which will be a content of Counter.t.sol. *)
val get_attacker : Concrete.Tseq.t -> string
