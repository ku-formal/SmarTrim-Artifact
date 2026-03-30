(** {!Dfa}. Handle algorithms about finite automata and regexs. *)

(** Erases all comments from the source code. *)
val kill_comment : string -> string

(** [kill_keyword kwd src] erases all [kwd] from [src]. Works well even if keywords and
    operator-like letters (e.g. [(]) are not seperated by spaces. *)
val kill_keyword : ?by:string -> string -> string -> string

val kill_selfdestruct : ?by:string -> string -> string
val pragma_version_pat : Re.t
val pragma_solidity_pat : Re.t
val is_pragma_version : string -> bool

(** check whether the input is in the form of [pragma solidity #x.y.z]. *)
val is_pragma_solidity : string -> bool

val kill_pragma_solidity : string -> string
