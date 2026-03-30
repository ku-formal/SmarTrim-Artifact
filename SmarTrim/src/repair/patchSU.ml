open Patch
open Frontend.Lang

let generate : Global.t -> Pgm.t -> Func.t -> patch_comp list
= fun global pgm f ->
  PatchACC.report_aware_template global pgm f
