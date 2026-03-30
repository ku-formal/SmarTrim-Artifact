let print j = print_endline (Yojson.Basic.show j) (* for debugging *)

let mk_err_report ~filename ~errmsg ~time ?(reportdir = "./output") () =
  let j =
    `Assoc
      [
        ("fileName", `String filename);
        ("baseName", `String (Filename.basename filename));
        ("time", `Float time);
        ("datetime", `String (Misc.get_datetime_string (Unix.gettimeofday ())));
        ("errMsg", `String errmsg);
        ("result", `List []);
      ]
  in
  let reportfile =
    Filename.concat reportdir (BatString.rchop ~n:4 (Filename.basename filename) ^ ".json")
  in
  let f = open_out reportfile in
  Printf.fprintf f "%s" (Yojson.Basic.pretty_to_string j);
  close_out f
;;
