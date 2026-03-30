let%test_module _ =
  (module struct
    open Dfa

    let%test "pragmasol_yes1" = is_pragma_solidity "pragma solidity 0.4.11;"
    let%test "pragmasol_yes2" = is_pragma_solidity "pragma   solidity <= 0.4. 26;"
    let%test "pragmasol_yes2" = is_pragma_solidity "pragma solidity^0.    4.        442;"
    let%test "pragmasol_no1" = not @@ is_pragma_solidity "pragma solidity =<0. 137.532;"
    let%test "pragmasol_no2" = not @@ is_pragma_solidity "pragma solidity 0.4.25"
    let%test "pragmasol_no3" = not @@ is_pragma_solidity "pragmasolidity 0.4.25;"

    let%test "erasure1" =
      kill_keyword "private" "function whatever private()" = "function whatever ()"
    ;;

    let%test "erasure2" =
      kill_keyword "private" "function whatever private returns()" = "function whatever  returns()"
    ;;
  end)
;;

let%test_module _ =
  (module struct
    open Solc.Ver

    let%test "accepted1" = of_string "0.4.11" = Some (mk 0 4 11)
    let%test "accepted2" = of_string "422.422.422000000" = Some (mk 422 422 422000000)
    let%test "denial1" = of_string "0.4.1.2" = None
    let%test "denial2" = of_string "0.4.a" = None
    let%test "eq" = mk 0 8 17 = mk 0 8 17
    let%test "lt" = mk 0 4 15 < mk 0 8 0
    let%test "gt" = mk 0 6 4 > mk 0 4 26
  end)
;;
