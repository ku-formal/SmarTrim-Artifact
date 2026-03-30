(** Some custom random & hash Library. *)

module Magic = Magic

module Xorshift = struct
  let magic =
    [|
      (0, 0, 0);
      (* 0 *)
      (0, 0, 0);
      (0, 0, 0);
      (1, 2, 2);
      (1, 3, 2);
      (2, 3, 4);
      (* 5 *)
      (1, 3, 4);
      (3, 4, 5);
      (3, 5, 4);
      (4, 2, 7);
      (4, 3, 7);
      (* 10 *)
      (4, 7, 10);
      (3, 7, 7);
      (8, 5, 12);
      (3, 5, 9);
      (3, 5, 12);
      (* 15 *)
      (4, 3, 7);
      (5, 8, 7);
      (3, 9, 11);
      (7, 11, 8);
      (5, 3, 13);
      (* 20 *)
      (7, 6, 10);
      (3, 5, 9);
      (5, 3, 17);
      (6, 5, 11);
      (5, 9, 13);
      (* 25 *)
      (5, 15, 6);
      (6, 2, 13);
      (5, 3, 7);
      (5, 3, 12);
      (5, 7, 23);
      (* 30 *)
      (5, 9, 7);
      (5, 17, 13);
      (5, 16, 22);
      (5, 11, 10);
      (5, 13, 10);
      (* 35 *)
      (6, 5, 13);
      (5, 7, 13);
      (7, 9, 14);
      (7, 9, 23);
      (7, 11, 19);
      (* 40 *)
      (7, 4, 27);
      (7, 13, 9);
      (7, 13, 12);
      (7, 11, 18);
      (11, 5, 27);
      (* 45 *)
      (11, 9, 41);
      (11, 8, 39);
      (11, 5, 12);
      (11, 5, 12);
      (11, 9, 24);
      (* 50 *)
      (11, 5, 35);
      (12, 5, 21);
      (12, 3, 14);
      (12, 15, 19);
      (12, 3, 19);
      (* 55 *)
      (12, 5, 33);
      (12, 7, 20);
      (12, 5, 25);
      (12, 6, 29);
      (12, 11, 27);
      (* 60 *)
    |]
  ;;

  type state = { state : int; bit : int }

  let make ?(seed = 1) bit : state =
    let mask = (1 lsl bit) - 1 in
    let seed = seed land mask in
    if seed = 0 then raise @@ Invalid_argument "xorshift : Invalid seed";
    if bit < 3 || bit >= Array.length magic then
      raise @@ Invalid_argument "xorshift : Unsupported bit number";
    { state = seed; bit }
  ;;

  let next (state : state) : int * state =
    let mask = (1 lsl state.bit) - 1 in
    let ( << ) n b = (n lsl b) land mask in
    let ( >> ) n b = n lsr b in
    let x = state.state in
    let a, b, c = magic.(state.bit) in
    let x = x lxor (x << a) in
    let x = x lxor (x >> b) in
    let x = x lxor (x << c) in
    (x, { state with state = x })
  ;;
end

module Cyclic = struct
  type t = { state : Xorshift.state; ubd : int; start : int; loopcnt : int }

  let make ?(seed = 1) ubd : t =
    if ubd <= 0 then invalid_arg "randutil : Invalid upper bound";
    let rec bit n acc = if n = 0 then acc else bit (n / 2) (acc + 1) in
    let bit = bit ubd 0 in
    let bit = max 3 bit in
    let state = Xorshift.make ~seed bit in
    { state; ubd; start = state.state; loopcnt = 0 }
  ;;

  let rec next (t : t) : int * t =
    let x, state = Xorshift.next t.state in
    let t = { t with state; loopcnt = t.loopcnt + Bool.to_int (x = t.start) } in
    if x <= t.ubd then (x, t) else next t
  ;;
end
