val trans_str_to_typeName : string -> Typ.t
val param_name : string
val gen_param_name : unit -> string
val run : Yojson.Basic.t -> string list -> Lang.Pgm.t
