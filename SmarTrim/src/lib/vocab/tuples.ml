(** Utility functions for tuple operations. *)

open Misc

module T2 = struct
  type ('a, 'b) t = 'a * 'b [@@deriving show]

  let mk x1 x2 = (x1, x2)
  let _1 (x1, _) = x1
  let _2 (_, x2) = x2
  let swap (a, b) = (b, a)
  let map f1 f2 (x1, x2) = (f1 x1, f2 x2)
  let mapn f = map f f
  let map1 f1 = map f1 id
  let map2 f2 = map id f2
  let curry f a b = f (a, b)
  let uncurry f (a, b) = f a b
  let f21 f _2 _1 = f _1 _2

  let compare ?(cmp1 = Stdlib.compare) ?(cmp2 = Stdlib.compare) (x1, x2) (y1, y2) =
    let r1 = cmp1 x1 y1 in
    match r1 with 0 -> cmp2 x2 y2 | _ -> r1
  ;;

  let cmpn c = compare ~cmp1:c ~cmp2:c
  let print p1 p2 fp ((a, b) : ('a, 'b) t) = Printf.fprintf fp "(%a, %a)" p1 a p2 b
  let printn p = print p p
  let to_list (a, b) = [ a; b ]
  let of_list = function [ a; b ] -> Some (a, b) | _ -> None
  let of_list_exn = function [ a; b ] -> (a, b) | _ -> invalid_arg "T2.of_list_exn"
  let lift_binop f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2)
end

module T3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c [@@deriving show]

  let make a b c = (a, b, c)
  let _1 (a, _b, _c) = a
  let _2 (_a, b, _c) = b
  let _3 (_a, _b, c) = c
  let _12 (a, b, _c) = (a, b)
  let _13 (a, _b, c) = (a, c)
  let _23 (_a, b, c) = (b, c)
  let map f1 f2 f3 (x1, x2, x3) = (f1 x1, f2 x2, f3 x3)
  let mapn f = map f f f
  let map1 f = map f id id
  let map2 f = map id f id
  let map3 f = map id id f
  let curry f a b c = f (a, b, c)
  let uncurry f (a, b, c) = f a b c
  let f132 f _1 _3 _2 = f _1 _2 _3
  let f213 f _2 _1 _3 = f _1 _2 _3
  let f231 f _2 _3 _1 = f _1 _2 _3
  let f312 f _3 _1 _2 = f _1 _2 _3
  let f321 f _3 _2 _1 = f _1 _2 _3

  let compare ?(cmp1 = Stdlib.compare) ?(cmp2 = Stdlib.compare) ?(cmp3 = Stdlib.compare)
      (x1, x2, x3) (y1, y2, y3) =
    let r1 = cmp1 x1 y1 in
    if r1 = 0 then
      let r2 = cmp2 x2 y2 in
      if r2 = 0 then cmp3 x3 y3 else r2
    else r1
  ;;

  let cmpn c = compare ~cmp1:c ~cmp2:c ~cmp3:c
  let eq1 ?(eq = ( = )) (l1, _, _) (r1, _, _) = eq l1 r1
  let eq2 ?(eq = ( = )) (_, l2, _) (_, r2, _) = eq l2 r2
  let eq3 ?(eq = ( = )) (_, _, l3) (_, _, r3) = eq l3 r3

  let print p1 p2 p3 fp ((a, b, c) : ('a, 'b, 'c) t) =
    Printf.fprintf fp "(%a, %a, %a)" p1 a p2 b p3 c
  ;;

  let printn p = print p p p
  let to_string f g h (a, b, c) = Printf.sprintf "(%s, %s, %s)" (f a) (g b) (h c)
  let to_list (a, b, c) = [ a; b; c ]
  let of_list = function [ a; b; c ] -> Some (a, b, c) | _ -> None
  let of_list_exn = function [ a; b; c ] -> (a, b, c) | _ -> invalid_arg "T3.of_list_exn"
  let lift_binop f (a1, a2, a3) (b1, b2, b3) = (f a1 b1, f a2 b2, f a3 b3)
end

module T4 = struct
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd [@@deriving show]

  let map f1 f2 f3 f4 (x1, x2, x3, x4) = (f1 x1, f2 x2, f3 x3, f4 x4)
  let map1 f1 (x1, x2, x3, x4) = (f1 x1, x2, x3, x4)
  let map2 f2 (x1, x2, x3, x4) = (x1, f2 x2, x3, x4)
  let map3 f3 (x1, x2, x3, x4) = (x1, x2, f3 x3, x4)
  let map4 f4 (x1, x2, x3, x4) = (x1, x2, x3, f4 x4)
  let mapn f = map f f f f

  let compare ?(cmp1 = Stdlib.compare) ?(cmp2 = Stdlib.compare) ?(cmp3 = Stdlib.compare)
      ?(cmp4 = Stdlib.compare) (x1, x2, x3, x4) (y1, y2, y3, y4) =
    let r1 = cmp1 x1 y1 in
    if r1 = 0 then
      let r2 = cmp2 x2 y2 in
      if r2 = 0 then
        let r3 = cmp3 x3 y3 in
        if r3 = 0 then cmp4 x4 y4 else r3
      else r2
    else r1
  ;;

  let cmpn ?(cmp = Stdlib.compare) = compare ~cmp1:cmp ~cmp2:cmp ~cmp3:cmp
  let _1 (a, _, _, _) = a
  let _2 (_, b, _, _) = b
  let _3 (_, _, c, _) = c
  let _4 (_, _, _, d) = d
  let _12 (a, b, _, _) = (a, b)
  let _13 (a, _, c, _) = (a, c)
  let _14 (a, _, _, d) = (a, d)
  let _23 (_, b, c, _) = (b, c)
  let _24 (_, b, _, d) = (b, d)
  let _34 (_, _, c, d) = (c, d)
  let _123 (a, b, c, _) = (a, b, c)
  let _124 (a, b, _, d) = (a, b, d)
  let _134 (a, _, c, d) = (a, c, d)
  let _234 (_, b, c, d) = (b, c, d)

  let print p1 p2 p3 p4 fp ((a, b, c, d) : ('a, 'b, 'c, 'd) t) =
    Printf.fprintf fp "(%a, %a, %a, %a)" p1 a p2 b p3 c p4 d
  ;;

  let printn p = print p p p p
  let to_list (a, b, c, d) = [ a; b; c; d ]
  let of_list = function [ a; b; c; d ] -> Some (a, b, c, d) | _ -> None
  let of_list_exn = function [ a; b; c; d ] -> (a, b, c, d) | _ -> invalid_arg "T4.of_list_exn"
  let lift_binop f (a1, a2, a3, a4) (b1, b2, b3, b4) = (f a1 b1, f a2 b2, f a3 b3, f a4 b4)
end

module T5 = struct
  let _1 (a, _, _, _, _) = a
  let _2 (_, b, _, _, _) = b
  let _3 (_, _, c, _, _) = c
  let _4 (_, _, _, d, _) = d
  let _5 (_, _, _, _, e) = e
  let to_list (a, b, c, d, e) = [ a; b; c; d; e ]
  let of_list = function [ a; b; c; d; e ] -> Some (a, b, c, d, e) | _ -> None

  let of_list_exn = function
    | [ a; b; c; d; e ] -> (a, b, c, d, e)
    | _ -> invalid_arg "T5.of_list_exn"
  ;;

  let lift_binop f (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) =
    (f a1 b1, f a2 b2, f a3 b3, f a4 b4, f a5 b5)
  ;;
end

module T6 = struct
  type ('a, 'b, 'c, 'd, 'e, 'f) t = 'a * 'b * 'c * 'd * 'e * 'f

  let to_list (a, b, c, d, e, f) = [ a; b; c; d; e; f ]
  let of_list = function [ a; b; c; d; e; f ] -> Some (a, b, c, d, e, f) | _ -> None

  let of_list_exn = function
    | [ a; b; c; d; e; f ] -> (a, b, c, d, e, f)
    | _ -> invalid_arg "T5.of_list_exn"
  ;;

  let lift_binop f (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6) =
    (f a1 b1, f a2 b2, f a3 b3, f a4 b4, f a5 b5, f a6 b6)
  ;;
end
