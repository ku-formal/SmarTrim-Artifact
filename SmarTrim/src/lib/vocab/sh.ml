(** shell *)

(** Do the same thing with [rm -rf <name>].*)
let rec rm_rf path =
  if Sys.file_exists path then begin
    match Sys.is_directory path with
    | true ->
      Sys.readdir path |> Array.iter (fun name -> rm_rf (Filename.concat path name));
      Unix.rmdir path
    | false -> Sys.remove path
  end
;;

let mv ~src ~dst = Sys.rename src dst
