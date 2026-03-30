open Frontend
open Formula

let eq l r = VBinRel (VEq, l, r)
let gt l r = VBinRel (VGt, l, r)
let mk_or a b = VOr (a, b)
let j = join_conjuncts
let z s = Z.of_string s
let int n = VInt n
let bytes32 n = VCast (EType (Bytes 32), VInt (z n))
let ite i t e = Ite (VCond i, t, e)
let read a i = Read (a, VInt i)

module ARG_S = struct
  let inputs =
    [|
      "";
      "b";
      "f9";
      "310";
      "9d4d";
      "cdae7";
      "2ef31d";
      "304891a";
      "4389f1cd";
      "59b569ef8";
      "7b24e31c82";
      "e74776365";
    |]
  ;;

  let outputs =
    let f s =
      let t = Digestif.KECCAK_256.digest_string s in
      Digestif.KECCAK_256.to_hex t
    in
    Array.map f inputs |> Array.map (( ^ ) "0x")
  ;;

  let keccak = Var.mk "@KECCAK256_S" (Mapping (UInt 256, EType (Bytes 32)))
  let keccak_e = VVar keccak

  let keccak_facts =
    List.init (Array.length inputs) (fun i -> eq (read keccak_e (Z.of_int i)) (bytes32 outputs.(i)))
    |> j
  ;;

  let keccak_domain_base = Var.mk "@KECCAK256_S_DOMAIN_BASE" (Mapping (String, EType (UInt 256)))
  let keccak_domain = Var.mk "@KECCAK256_S_DOMAIN" (Mapping (String, EType (UInt 256)))
  let keccak_domain_e = VVar keccak_domain

  let keccak_domain_facts =
    let i : Var.t = ("@i1", EType String) in
    let base_fact =
      ForAll
        ( [ i ],
          VBinRel
            (VEq, Read (VVar keccak_domain_base, VVar i), VInt (Z.of_int (Array.length inputs))) )
    in
    let folder acc i = Write (acc, Str (inputs.(i), EType String), VInt (Z.of_int i)) in
    let e = List.fold_left folder (VVar keccak_domain_base) (List.init (Array.length inputs) id) in
    let cumul_fact = VBinRel (VEq, keccak_domain_e, e) in
    VAnd (base_fact, cumul_fact)
  ;;

  let run vf e1 =
    let i = gen_newsym (EType (UInt 256)) in
    let already_know_array_facts = free_vf vf |> Set.mem keccak in
    let vf =
      if already_know_array_facts then vf else VAnd (vf, VAnd (keccak_facts, keccak_domain_facts))
    in
    let assumption = gt (VInt (Z.of_int (Array.length inputs))) (VVar i) in
    let vf = VAnd (vf, assumption) in
    let assumption = eq (VVar i) (Read (keccak_domain_e, e1)) in
    let vf = VAnd (vf, assumption) in
    let output = Read (keccak_e, VVar i) in
    (vf, output)
  ;;
end

module ARG_B16AAU = struct
  let addrs = Array.map z [| "0x111"; "0x222" |]
  let uints = Array.map z [| "0"; "600000000000000000"; "1000000000000000000" |]
  let byte16s = Array.map z [| "0x1" |]

  let condition1 a b c d =
    j [ eq a (int byte16s.(1)); eq b (int addrs.(1)); eq c (int addrs.(2)); eq d (int uints.(1)) ]
  ;;

  let condition2 a b c d =
    j [ eq a (int byte16s.(1)); eq b (int addrs.(1)); eq c (int addrs.(1)); eq d (int uints.(2)) ]
  ;;

  let condition3 a b c d =
    j [ eq a (int byte16s.(1)); eq b (int addrs.(1)); eq c (int addrs.(2)); eq d (int uints.(3)) ]
  ;;

  let outs =
    Array.map bytes32
      [|
        "0x5dbf7fcec09ae7957550174580f0415015f0e9a5b1985a8bd55613aa9bff2c40";
        "0xfaf9ec944ffac469e076d6ce56060c5d3efb34b6ab7b2c225d78ac5215568db1";
        "0x19900eeb85ea846013af468c24b22b5f08e1dd47e0bb561c5ed03e30d61eee1d";
      |]
  ;;

  let assumption a b c d =
    mk_or (condition1 a b c d) (mk_or (condition2 a b c d) (condition3 a b c d))
  ;;

  let output a b c d =
    ite (condition1 a b c d) outs.(0) (ite (condition2 a b c d) outs.(1) outs.(2))
  ;;

  let run vf a b c d =
    let vf = VAnd (vf, assumption a b c d) in
    let output = output a b c d in
    (vf, output)
  ;;
end

let keccak256_s = ARG_S.keccak

let add_assumption_and_get_output_exp (vf : vformula) (arg_exp_list : vexp list) =
  let typs = List.map get_typ_vexp arg_exp_list in
  match typs with
  | [ EType String ] ->
    let e = List.hd arg_exp_list in
    Some (ARG_S.run vf e)
  | [ EType (Bytes 16); EType Address; EType Address; EType (UInt 256) ] ->
    let a, b, c, d = T4.of_list_exn arg_exp_list in
    Some (ARG_B16AAU.run vf a b c d)
  | _ -> None
;;
