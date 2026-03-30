include Vocab_ropas
include Tuples
include Misc
module Array_immut = Array_immut
module Batteries = Batteries_replace

module Counter = struct
  (** Thread safe counter *)

  module type S = sig
    val gen : unit -> int
    val reset : unit -> unit
  end

  module M () : S = struct
    let count = ref 0
    let mutex = Semaphore.Binary.make true

    let gen () =
      Semaphore.Binary.acquire mutex;
      let n = !count in
      incr count;
      Semaphore.Binary.release mutex;
      n
    ;;

    let reset () =
      Semaphore.Binary.acquire mutex;
      count := 0;
      Semaphore.Binary.release mutex
    ;;
  end
end

module Field = Base.Field
module Json = Json
module Os = Os
module Pp = Pp
module Sh = Sh
module Shadow_batteries = Shadow_batteries

module Shadow_vocab = struct
  module Vocab = struct end
end

module Variant = Variantslib.Variant

module Z = struct
  include Z

  let pp = pp_print

  module Set = Batteries.Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Map = Batteries.Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end
