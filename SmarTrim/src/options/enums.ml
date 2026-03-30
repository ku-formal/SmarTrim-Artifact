module Exit_code = struct
  type t =
    | Succeed
    | General_error
    | Compile_error
    | Unsupported_solc
    | Solution_inv_not_hold
    | Test_failed
  [@@deriving show { with_path = false }]

  let to_int = function
    | Succeed -> 0
    | General_error -> 1
    | Compile_error -> 2
    | Unsupported_solc -> 3
    | Solution_inv_not_hold -> 4
    | Test_failed -> 88
  ;;

  let exit n = exit (to_int n)
end

module Mode = struct
  type t = Verify | Exploit | Repair | Bugsynth [@@deriving show { with_path = false }]
end

module Search_strategy = struct
  type t = Increasing_order | Uniform_random_pop | Random_pop | Random | Ngram of int
  [@@deriving show { with_path = false }, equal]

  (** Tell {!Climate} how to handle search strategies. *)
  let conv =
    let open Climate.Arg_parser in
    enum ~default_value_name:"inc" ~eq:equal
      [
        ("inc", Increasing_order);
        ("urandom-pop", Uniform_random_pop);
        ("random-pop", Random_pop);
        ("random", Random);
        ("ngram2", Ngram 2);
        ("ngram3", Ngram 3);
        ("ngram4", Ngram 4);
      ]
  ;;
end
