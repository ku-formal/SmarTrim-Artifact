open! Frontend
open Frontend.Lang
open! Vocab

type t = Node.t option * Paths.t

let get_ctx (nop, _) = nop
let get_fkey (_, p) = Paths.fkey p
let get_bp (_, p) = Paths.basic_path p

let to_string (nop, p) =
  match nop with
  | None -> Paths.to_string p
  | Some n -> "(" ^ Node.to_string n ^ ", " ^ Paths.to_string p ^ ")"
;;

module Set = BatSet.Make (struct
  type nonrec t = t

  let compare = Stdlib.compare
end)
