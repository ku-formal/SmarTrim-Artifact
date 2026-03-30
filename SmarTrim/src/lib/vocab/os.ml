open struct
  let ( let* ) = Result.bind
  let option_to_result opt = match opt with Some s -> Ok s | None -> Error (Unix.WEXITED 0)
  let hd_result l = match l with h :: _ -> Ok h | _ -> Error (Unix.WEXITED 0)
end

(** I love devnull since it destroys everything *)
let devnull : Unix.file_descr = Unix.openfile Filename.null [ Unix.O_RDWR ] 0o666

(** Get the location of home, which is usually denoted as [~] or [$HOME].

    Note: This function is maybe logically correct, however it is not tested on the Windows. Please
    somebody help me *)
let get_home () = try Unix.getenv "HOME" with Not_found -> Unix.(getpwnam "username").pw_dir

(** Project directory. Always returns an absolute path. *)
let project_dir =
  let project_dir = Filename.dirname Sys.argv.(0) in
  if Filename.is_relative project_dir then Filename.concat (Unix.getcwd ()) project_dir
  else project_dir
;;

let somewhat_randomly_picked_number = 88

(** Validate that [Sys.argv.(0)] really denotes this program's path. We need this assertion because
    we find the location of this program by [Sys.argv.(0)]. *)
let validate_wellformed_argv0 () =
  let n = Sys.command (Sys.argv.(0) ^ " noticeme 1") in
  if n <> somewhat_randomly_picked_number then begin
    Pp.failwithf "FATAL: Sys.argv.(0) (%s) does not give the correct location of this program"
      Sys.argv.(0)
  end
;;

(** Filename concatenation. [cat [s1; s2; ...]]
    @raise Invalid_argument on empty list. *)
let cat = BatList.reduce Filename.concat

(** Filename concatenation, but starts from the project root directory. *)
let cat_fromroot l = cat (project_dir :: l)

let cat_fromhome l = cat (get_home () :: l)

let pipe () =
  let in_file_descr, out_file_descr = Unix.pipe () in
  let in_channel = Unix.in_channel_of_descr in_file_descr in
  let out_channel = Unix.out_channel_of_descr out_file_descr in
  (in_channel, out_channel)
;;

let system_call_to_sl cmd =
  let ic = Unix.open_process_in cmd in
  let line = In_channel.input_lines ic in
  let exitted = Unix.close_process_in ic in
  match exitted with WEXITED 0 -> Ok line | _ -> Error exitted
;;

let lwt_system_call_to_bytes cmd = Lwt_process.with_process_in cmd (fun ic -> Lwt_io.read ic#stdout)

(** {b Note} When using this function, always be aware of the fact that the endline is not
    automatically erased. *)
let system_call_to_bytes cmd =
  let ic = Unix.open_process_in cmd in
  let line = In_channel.input_all ic in
  let exitted = Unix.close_process_in ic in
  match exitted with WEXITED 0 -> Ok line | _ -> Error exitted
;;

let system_call_assert cmd =
  let ic = Unix.open_process_in cmd in
  let exitted = Unix.close_process_in ic in
  match exitted with WEXITED 0 -> Ok () | WEXITED n -> Error n | _ -> Error 0
;;

let lwt_system_call_assert ?(errmsg = "") cmd =
  let open Lwt.Syntax in
  let* status = Lwt_process.exec cmd in
  match status with WEXITED 0 -> Lwt.return_unit | _ -> failwith errmsg
;;

(** in kilobytes. Currently only supports linux/mac. *)
let memory_used () =
  let* mem = system_call_to_sl "ps -o rss= -p $$" in
  let* mem = hd_result mem in
  let* value = int_of_string_opt mem |> option_to_result in
  Ok value
;;

let lwt_list_map ?(ncores = 0) (f : 'a -> 'b) l =
  match ncores with
  | 0 ->
    let f x = Lwt.return (f x) in
    let l = Lwt_list.map_p f l in
    Lwt_main.run l
  | 1 -> List.map f l
  | n ->
    let f x = Lwt.return (f x) in
    let pool = Lwt_pool.create n Lwt.return in
    let process_with_pool item = Lwt_pool.use pool (fun () -> f item) in
    let l = Lwt_list.map_p process_with_pool l in
    Lwt_main.run l
;;

(** we support these three operating systems *)
module Os_type = struct
  type t = Linux | MacOS | Windows [@@deriving show { with_path = false }, compare, equal]

  let get () : t =
    match Sys.os_type with
    | "Unix" -> begin
      match system_call_to_sl "uname" with Ok [ "Darwin" ] -> MacOS | _ -> Linux
    end
    | "Win32" | "Cygwin" -> Windows
    | _ -> failwith "Only Linux | MacOS | Windows are supported"
  ;;
end

let get_platform_name () =
  let platform = Os_type.get () in
  match platform with
  | Linux -> "linux-amd64"
  | MacOS -> "macosx-amd64"
  | Windows -> "windows-amd64"
;;

(** Utilities about OS. As OCaml's {!Unix} module lacks some features, we simply use the Python
    binding... *)
module Pybind : sig
  val request : string -> (string, Unix.process_status) result
  val request_bytes : string -> (string, Unix.process_status) result
  val lwt_request_bytes : string -> string Lwt.t
end = struct
  let request_src =
    {%string|import requests
import sys
got = requests.get(sys.argv[1])
if got.status_code != 200:exit(got.status_code)
print(got.content.decode())
exit(0)
|}
  ;;

  let request url =
    let cmd = {%string|python3 -c '%{request_src}' %{url}|} in
    Result.map (String.concat "\n") (system_call_to_sl cmd)
  ;;

  let request_bytes_src =
    {%string|import requests
import sys
got = requests.get(sys.argv[1])
if got.status_code != 200:exit(got.status_code)
sys.stdout.buffer.write(got.content)
sys.stdout.buffer.flush()
exit(0)
|}
  ;;

  let request_bytes url =
    let cmd = {%string|python3 -c '%{request_bytes_src}' %{url}|} in
    system_call_to_bytes cmd
  ;;

  let lwt_request_bytes url =
    let cmd = ("python3", [| "python3"; "-c"; request_bytes_src; url |]) in
    lwt_system_call_to_bytes cmd
  ;;
end

module Filelock : sig
  type t

  val lock : string -> t
  val try_lock : string -> t option
  val unlock : t -> unit
end = struct
  type t = { oc : out_channel }

  let lock filepath : t =
    let oc = open_out filepath in
    let fd = Unix.descr_of_out_channel oc in
    Unix.lockf fd F_LOCK 0;
    { oc }
  ;;

  let try_lock filepath : t option =
    let oc = open_out filepath in
    let fd = Unix.descr_of_out_channel oc in
    try
      Unix.lockf fd F_TLOCK 0;
      Some { oc }
    with Unix.Unix_error _ -> None
  ;;

  let unlock filelock =
    let fd = Unix.descr_of_out_channel filelock.oc in
    Unix.lockf fd F_ULOCK 0;
    close_out filelock.oc
  ;;
end
