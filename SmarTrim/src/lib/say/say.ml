(** {b Example:} [compact_string ~maxlen:7 "smartest" = "smart.."] *)
let compact_string ?(maxlen = 60) (s : string) =
  if String.length s > maxlen then Printf.sprintf "%s.." (String.sub s 0 (maxlen - 2)) else s
;;

let saydata1 : string ref = ref ""
let saydata2 : string ref = ref ""
let working = ref false
let verbosity : int ref = ref 0

let turn_on () =
  if Unix.isatty Unix.stdout then begin
    if !working then failwith "Duplicated Say.turn_off";
    working := true;
    Format.printf "\n\n%!"
  end
;;

let turn_off () =
  if Unix.isatty Unix.stdout then begin
    if not !working then failwith "Duplicated Say.turn_off";
    working := false;
    Format.printf "\027[F\027[K\027[F\027[K%!"
  end
;;

let set_verbosity n = verbosity := n
let get_verbosity () = !verbosity

(** Print the string at the 1st line of the [Say] workspace. *)
let say1 s =
  if Unix.isatty Unix.stdout && !working then begin
    saydata1 := s;
    Format.printf "\027[F\027[K\027[F\027[K%s\n%s\n%!" (compact_string !saydata1)
      (compact_string !saydata2)
  end
;;

(** Print the string at the 2nd line of the [Say] workspace. *)
let say2 s =
  if Unix.isatty Unix.stdout && !working then begin
    saydata2 := s;
    Format.printf "\027[F\027[K\027[F\027[K%s\n%s\n%!" (compact_string !saydata1)
      (compact_string !saydata2)
  end
;;

(** Print the string directly above the [Say] workspace. *)
let say ~lv s =
  if lv > !verbosity then ()
  else begin
    if Unix.isatty Unix.stdout && !working then
      Format.printf "\027[F\027[K\027[F\027[K%s\n%s\n%s\n%!" s (compact_string !saydata1)
        (compact_string !saydata2)
    else Format.printf "%s\n%!" s
  end
;;

let pp ~lv fmt = Vocab.Pp.kspf (say ~lv) fmt
