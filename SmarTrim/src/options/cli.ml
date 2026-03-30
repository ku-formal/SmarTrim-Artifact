open! Vocab
open Climate
module C = Options_

(** optional assignment operator. *)
let ( /:= ) r v = match v with Some v -> r := v | None -> ()

let common_args =
  let open Arg_parser in
  let+ input = pos_req 0 string ~doc:"Input Solidity file." ~value_name:"FILE"
  and+ main =
    named_opt [ "m"; "main" ] string ~doc:"Name of the main contract to be deployed"
      ~value_name:"CONTRACT"
  and+ kind =
    named_with_default [ "k"; "kind" ] (list string) ~default:[ "all" ]
      ~doc:"Kind of vulnerabilities, separated by ','. Support: all,io,dz,as,el,su,erc20"
      ~value_name:"KIND"
  and+ report = flag [ "r"; "report" ] ~doc:"Produce analysis/repair reports"
  and+ quiet = flag [ "quiet" ] ~doc:"Quiet running. This supresses the verbosity argument"
  and+ verbosity =
    flag_count [ "v"; "verbose" ]
      ~doc:
        "Verbosity. Default: 1, Max: 3. Pass multiple times to increase verbosity (example: -vvvvv)"
  and+ outdir =
    named_opt [ "o"; "outdir" ] string
      ~doc:"Directory in which outputs are stored. If this is set, '--report' is automatically set"
      ~value_name:"DIR"
  and+ solv =
    named_opt [ "solv"; "solc-ver" ] string ~doc:"Specify solidity compiler version, e.g., 0.5.13"
      ~value_name:"VER"
  and+ no_dsc =
    flag [ "no-dsc" ]
      ~doc:
        "Turn off the delayed safety checking mode; does not generate VCs that consider normal \
         terminations of transactions."
  and+ max_ocaml_memory =
    named_with_default [ "threshold-memory" ] int ~default:(3 * 1024)
      ~doc:
        "Threshold Ocaml memory in MB. If exceeds, the program will not generate any new elements \
         anymore. Currently only supported in exploit mode. Note that solver memory is currently \
         unhandlable. (default: 3072MB = 3GB)"
      ~value_name:"M"
  and+ inline_depth =
    named_opt [ "inline" ] int ~doc:"the number of times being inlined" ~value_name:"D"
  and+ debug =
    named_with_default [ "debug" ] string ~default:""
      ~doc:"[developer tool] debugging certain parts"
  and+ il =
    flag [ "il"; "intermediate-lang" ]
      ~doc:"Show interpretation to VeriSmart intermediate language and exit."
  and+ cfg = flag [ "cfg" ] ~doc:"show interpretation to VeriSmart control flow graph and exit." in
  (* common arg parser behaviours begin *)
  C.input := input;
  C.main_contract /:= main;
  List.iter C.kind_setter kind;
  C.mk_report := report;
  C.verbose := if quiet then 0 else Int.min 5 (Int.max 1 verbosity);
  (match outdir with
  | Some outdir ->
    C.mk_report := true;
    C.outdir := outdir
  | None -> ());
  if !C.mk_report && !C.outdir = "" then C.outdir := Vocab.Os.cat_fromroot [ "output" ];
  let solv = Option.map Solc.Ver.of_string_exn solv in
  C.solc_ver := solv;
  C.refined_vcgen := not no_dsc;
  C.max_ocaml_memory_allowed_in_mb := max_ocaml_memory * 1048576;
  C.inline_depth /:= inline_depth;
  C.debug := debug;
  C.intermediate_language := il;
  C.cfg := cfg;
  if !C.intermediate_language || !C.cfg then C.mk_report := false
;;

let verify =
  let open Arg_parser in
  let+ () = common_args
  and+ timeout =
    named_with_default [ "t"; "timeout" ] int ~default:300
      ~doc:"Total time budget for verification mode, in seconds (default: 300s)"
  and+ solver_timeout =
    named_with_default [ "s"; "solver-timeout" ] int ~default:30000
      ~doc:"Solver time budget for each call, in milliseconds (default: 30000 (=30s))"
      ~value_name:"T"
  and+ uninterp_nonlinear =
    flag [ "uninterp-nonlinear" ]
      ~doc:"[experimental] Use uninterpreted functions for nonlinear exps"
  and+ pow = flag [ "pow" ] ~doc:"When IO checker is on, verify power ops" in
  C.mode := Enums.Mode.Verify;
  C.verify_timeout := timeout;
  C.z3timeout := solver_timeout;
  C.uninterp_nonlinear := uninterp_nonlinear;
  C.pow := pow
;;

let exploit =
  let open Arg_parser in
  let+ () = common_args
  and+ timeout =
    named_with_default [ "t"; "timeout" ] int ~default:1800
      ~doc:"Total time budget for exploit generation mode, in seconds (default: 1800s)"
  and+ solver_timeout =
    named_with_default [ "s"; "solver-timeout" ] int ~default:90000
      ~doc:"Solver time budget for each call, in milliseconds (default: 90000 (=90s))"
      ~value_name:"T"
  and+ depth =
    named_with_default [ "d"; "depth" ] int ~default:1000
      ~doc:"Maximum transaction depth other than initial transaction (constructor call)"
      ~value_name:"D"
  and+ lm_path =
    named_opt [ "lm-path" ] string ~doc:"Directory of the statistical language model"
      ~value_name:"DIR"
  and+ validate =
    flag [ "V"; "validate" ] ~doc:"run the concrete validator after finding vulnerabilities."
  and+ init_eth =
    named_with_default [ "init-eth" ] int ~default:100
      ~doc:"the amount of initial Ethers of a contract to be analyzed (unit: ether) (default: 100)"
      ~value_name:"ETH"
  and+ send_eth =
    named_with_default [ "max-send-eth" ] int ~default:100000
      ~doc:
        "the amount of Ethers in which humans can send in one transaction (unit: ether) (default: \
         100000)"
      ~value_name:"ETH"
  and+ strategy =
    named_with_default [ "strategy" ] Enums.Search_strategy.conv ~default:Increasing_order
      ~doc:"search strategy. Support: inc,random-pop,urandom-pop,random,ngram2,ngram3,ngram4"
      ~value_name:"STRATEGY"
  and+ no_pruning =
    flag [ "no-pruning" ] ~doc:"does not perform pruning other than infeasible tseq pruning."
  and+ candidates =
    named_opt [ "c"; "candidates" ] string
      ~doc:"(experimental) candidates of vulnerability lines (input e.g.: 10,12,13)"
      ~value_name:"CANDY"
  and+ expectation =
    named_opt [ "expect" ] int
      ~doc:"How many queries are expected to be disproven and validated. Used for testing"
      ~value_name:"N"
  in
  (* exploit arg parser behaviours begin *)
  C.mode := Enums.Mode.Exploit;
  C.exploit_timeout := timeout;
  C.z3timeout := solver_timeout;
  C.E.transaction_depth := depth;
  C.E.lm_path /:= lm_path;
  C.validate := validate;
  if validate && (not !C.mk_report) && (not !C.intermediate_language) && not !C.cfg then
    failwith "--validate cannot be used without turning --report on";
  if init_eth < 0 then invalid_arg "invalid init_eth";
  (C.init_eth := Z.(mul (of_int init_eth) (pow (of_int 10) 18)));
  if send_eth < 0 then invalid_arg "invalid send_eth";
  (C.send_eth := Z.(mul (of_int send_eth) (pow (of_int 10) 18)));
  C.E.search_strategy := strategy;
  C.E.pruning := not no_pruning;
  C.candidates /:= candidates;
  (* outdir setting, for saving validation files *)
  let dirname = Filename.basename !C.input in
  let dirname = Filename.remove_extension dirname in
  let dirname = Os.cat [ !C.outdir; dirname ] in
  if !C.mk_report then begin
    Sh.rm_rf dirname;
    Sys.mkdir dirname 0o755
  end;
  C.outdir := dirname;
  C.E.expectation := expectation;
  ()
;;

let repair =
  let open Arg_parser in
  let+ () = common_args
  and+ no_learn =
    flag [ "no-learn" ]
      ~doc:"Do not perform online- and offline learning, i.e., search in increasing order"
  and+ no_diff =
    flag [ "no-diff" ]
      ~doc:"do not perform online- and offline learning, i.e., search in increasing order"
  and+ ksrc =
    named_with_default [ "ksrc" ] string ~default:""
      ~doc:"dir of knowlege source file for transfer learning" ~value_name:"PATH"
  and+ overify = flag [ "overify" ] ~doc:"do verification for original contract in repair mode"
  and+ pverify = flag [ "pverify" ] ~doc:"do patch verification"
  and+ compdir =
    named_with_default [ "compdir" ] string ~default:""
      ~doc:"comparison target directory for differential verification" ~value_name:"PATH"
  and+ report =
    named_with_default [ "repair-report" ] string ~default:""
      ~doc:"report file containing fix targets" ~value_name:"PATH"
  in
  C.mode := Enums.Mode.Repair;
  C.R.no_learn := no_learn;
  C.R.no_diff := no_diff;
  C.R.ksrc := ksrc;
  C.R.overify := overify;
  C.R.pverify := pverify;
  C.R.compdir := compdir;
  C.R.report := report;
  ()
;;

(** {b Note} this parser never returns *)
let solc_run : 'a Arg_parser.t =
  let open Arg_parser in
  let+ version = pos_req 0 string ~doc:"solc version." ~value_name:"VER"
  and+ args_left = pos_right 0 string ~doc:"arguments you want to pass." in
  let version = Solc.Ver.of_string_exn version in
  let path = Solc.get_solc_path version in
  let args = String.concat " " (path :: args_left) in
  let exited = Unix.system args in
  match exited with WEXITED exited -> exit exited | _ -> exit 77
;;

(** {b Note} this parser never returns *)
let solc_install : 'a Arg_parser.t =
  let open Arg_parser in
  let+ version =
    pos_req 0 string ~doc:"solc version, or \"all\" to install all possible version"
      ~value_name:"VER"
  and+ forced =
    flag [ "f"; "forced" ] ~doc:"install solc even when the solc is already installed"
  in
  Solc.Install.install_solc ~forced version;
  exit 0
;;

(** {b Note} this parser never returns *)
let solc : 'a Command.t =
  let open Command in
  group ~doc:"Install or run the solc binaries."
    [
      subcommand "install"
        (singleton ~doc:"Install solc. This command is platform independent." solc_install);
      subcommand "run"
        (singleton
           ~doc:
             "Call solc with the version you want. (e.g. ./main.exe solc 0.4.13 -- \
              --ast-compact-json CONTRACT.sol)"
           solc_run);
    ]
;;

(** {b Note} this parser never returns *)
let noticeme : 'a Arg_parser.t =
  let open Arg_parser in
  let+ n = pos_req 0 int in
  exit (n * Os.somewhat_randomly_picked_number)
;;

let main_prose =
  let open Manpage in
  prose ~description:[ `Pre "VeriSmart, a Solidity smart contracts analyzer." ] ()
;;

let parser =
  let open Command in
  group ~prose:main_prose ~doc:"VeriSmart, a Solidity smart contracts analyzer."
    [
      subcommand ~aliases:[ "v" ] "verify"
        (singleton verify ~doc:"Automatically prove the safety of possibly-vulnerable operations.");
      subcommand ~aliases:[ "e" ] "exploit"
        (singleton exploit
           ~doc:"Run symbolic execution to automatically generate vulnerable transaction sequences.");
      subcommand ~aliases:[ "r" ] "repair"
        (singleton repair ~doc:"Automatically repair buggy contracts.");
      subcommand "solc" solc;
      subcommand ~hidden:true "noticeme" (singleton noticeme);
    ]
;;

let help_style =
  let open Help_style in
  {
    program_doc = { ansi_style_plain with color = None };
    usage = { ansi_style_plain with color = None; bold = true };
    section_heading = { ansi_style_plain with color = None; bold = true };
    arg_name = { ansi_style_plain with color = None; bold = true };
    arg_doc = { ansi_style_plain with color = None };
    error = { ansi_style_plain with color = None };
  }
;;
