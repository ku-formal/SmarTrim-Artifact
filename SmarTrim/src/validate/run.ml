open Vd_util

open struct
  (** cleans the resources. Highly recommended to use with {!Fun.protect}. *)
  let clean_rsc (final_outdir : string) (config : config) : unit =
    Sh.rm_rf config.project_dir;
    close_out config.oc;
    let dirname = Filename.basename config.dir in
    let dirname = Os.cat [ final_outdir; dirname ] in
    try Sys.rename config.dir dirname
    with Sys_error _ ->
      let os_type = Os.Os_type.get () in
      if os_type = Linux || os_type = MacOS then
        let _ = Unix.system {%string|mv %{config.dir} %{dirname}|} in
        ()
  ;;
end

(** [main outdir solidity_content tseq_concrete]. Entrypoint of the {!Validate} module. *)
let main outdir solidity_content (tseq : Concrete.Tseq.t) =
  let config = init_space () in
  Fun.protect
    ~finally:(fun () -> clean_rsc outdir config)
    (fun () -> Foundrybuild.run config solidity_content tseq)
;;

let from_file outdir contract_path tseq_path =
  let tseq_json = Yojson.Safe.from_file tseq_path in
  let tseq = Reports.Exploit_r.Tseq.t_of_yojson tseq_json in
  let tseq = Concrete.Tseq.to_origin tseq in
  let solidity_content = In_channel.(with_open_text contract_path input_all) in
  main outdir solidity_content tseq
;;
