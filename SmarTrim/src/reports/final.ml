module Exploit = struct
  type t = (Exploit_r.Summary.t, Exploit_r.Tseq.t) Base_r.t [@@deriving yojson]

  module Ez = struct
    type t = (Exploit_r.Summary.t, Exploit_r.Tseq.t) Base_r.t [@@deriving yojson]
  end
end
