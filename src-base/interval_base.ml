(*
    Copyright 2011 Jean-Marc Alliot / Jean-Baptiste Gotteland
    Copyright 2018 Christophe Troestler

    This file is part of the ocaml interval library.

    The ocaml interval library is free software:
    you can redistribute it and/or modify it under the terms of
    the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The ocaml interval library is distributed in the hope that it will be
    useful,but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with the ocaml interval library.
    If not, see <http://www.gnu.org/licenses/>.
*)

module type T = sig
  type number
  type t

  val zero : t
  val one : t
  val pi: t
  val two_pi : t
  val half_pi : t
  val euler: t

  val entire : t
  val v : number -> number -> t
  val inf : t -> number
  val sup : t -> number
  val singleton : number -> t
  val of_int : int -> t

  val to_string : ?fmt: (number -> 'b, 'a, 'b) format -> t -> string
  val pr : out_channel -> t -> unit
  val pr_fmt : ?fmt: (number -> 'b, 'a, 'b) format ->
               out_channel -> t -> unit
  val pp : Format.formatter -> t -> unit
  val pp_fmt : ?fmt: (number -> 'b, 'a, 'b) format ->
               Format.formatter -> t -> unit
  val fmt : (number -> 'b, 'a, 'b) format -> (t -> 'c, 'd, 'e, 'c) format4

  val compare_f: t -> number -> int
  val belong : number -> t -> bool
  val is_bounded : t -> bool
  val is_entire : t -> bool
  val equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val subset : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val precedes : t -> t -> bool
  val interior : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val strict_precedes : t -> t -> bool
  val disjoint : t -> t -> bool

  val width: t -> t
  val width_up : t -> number
  val width_dw : t -> number
  val diam : t -> number
  val dist : t -> t -> t
  val dist_up : t -> t -> number
  val mag : t -> number
  val mig : t -> number
  val sgn: t -> t
  val truncate: t -> t
  val abs: t -> t
  val hull: t -> t -> t
  val inter_exn : t -> t -> t
  val inter : t -> t -> t option

  val max: t -> t -> t
  val min: t -> t -> t
  val ( + ) : t -> t -> t
  val ( +. ): t -> number -> t
  val ( +: ): number -> t -> t
  val ( - ): t -> t -> t
  val ( -. ): t -> number -> t
  val ( -: ): number -> t -> t
  val ( ~- ): t -> t
  val ( * ): t -> t -> t
  val ( *. ): number -> t -> t
  val ( *: ): t -> number -> t
  val ( / ): t -> t -> t
  val ( /. ): t -> number -> t
  val ( /: ): number -> t -> t
  val inv: t -> t
  type 'a one_or_two = One of 'a | Two of 'a * 'a
  val invx : t -> t one_or_two
  val cancelminus : t -> t -> t
  val cancelplus : t -> t -> t
  val ( ** ): t -> int -> t
  val sqrt : t -> t
end


(* [min] and [max], specialized to floats (faster).
   NaN do dot need to be handled (see [I.v]). *)
let[@inline] fmin (a: float) (b: float) = if a <= b then a else b
let[@inlne] fmax (a: float) (b: float) = if a <= b then b else a

let[@inline] is_even x = x land 1 = 0

(** Base [RoundDown] module *)
module RD = struct
  module U = Interval__U

  type t = float
  let zero = 0.
  let one = 1.
  let pi      = 0x1.921fb54442d18p1
  let two_pi  = 0x1.921fb54442d18p2
  let half_pi = 0x1.921fb54442d18p0
  let euler   = 0x1.5bf0a8b145769p1

  external float: (int [@untagged]) -> (float [@unboxed])
    = "ocaml_low_float_byte" "ocaml_low_float"
  external ( +. ): float -> float -> float
    = "ocaml_LOW_add_byte" "ocaml_LOW_add" [@@unboxed]
  external ( -. ): float -> float -> float
    = "ocaml_LOW_sub_byte" "ocaml_LOW_sub" [@@unboxed]
  external ( *. ): float -> float -> float
    = "ocaml_LOW_mul_byte" "ocaml_LOW_mul" [@@unboxed]
  external ( /. ): float -> float -> float
    = "ocaml_LOW_div_byte" "ocaml_LOW_div" [@@unboxed]
  external sqrt: float -> float
    = "ocaml_LOW_sqrt_byte" "ocaml_LOW_sqrt" [@@unboxed]

  let[@inline] sqr x = x *. x

  (* a·xⁿ for a ≥ 0, x ≥ 0 and n ∈ ℕ. *)
  let rec pos_pow_IN a x n =
    if n = 0 then a
    else if is_even n then pos_pow_IN a (x *. x) (n / 2)
    else pos_pow_IN (a *. x) (x *. x) (n / 2)

  let[@inline] dist (x: float) (y: float) =
    if x <= y then y -. x else x -. y
end

(** Base [RoundUp] module? *)
module RU = struct
  module U = Interval__U

  type t = float
  let zero = 0.
  let one = 1.
  let pi      = 0x1.921fb54442d19p1
  let two_pi  = 0x1.921fb54442d19p2
  let half_pi = 0x1.921fb54442d19p0
  let euler   = 0x1.5bf0a8b14576Ap1

  external float: (int [@untagged]) -> (float [@unboxed]) =
    "ocaml_high_float_byte" "ocaml_high_float"
  external ( +. ) : float -> float -> float
    = "ocaml_HIGH_add_byte" "ocaml_HIGH_add" [@@unboxed]
  external ( -. ): float -> float -> float
    = "ocaml_HIGH_sub_byte" "ocaml_HIGH_sub" [@@unboxed]
  external ( *. ): float -> float -> float
    = "ocaml_HIGH_mul_byte" "ocaml_HIGH_mul" [@@unboxed]
  external ( /. ): float -> float -> float
    = "ocaml_HIGH_div_byte" "ocaml_HIGH_div" [@@unboxed]
  external sqrt: float -> float
    = "ocaml_HIGH_sqrt_byte" "ocaml_HIGH_sqrt" [@@unboxed]

  let[@inline] sqr x = x *. x

  (* a·xⁿ for a ≥ 0, x ≥ 0 and n ∈ ℕ. *)
  let rec pos_pow_IN a x n =
    if n = 0 then a
    else if is_even n then pos_pow_IN a (x *. x) (n / 2)
    else pos_pow_IN (a *. x) (x *. x) (n / 2)

  let[@inline] dist (x: float) (y: float) =
    if x <= y then y -. x else x -. y
end

let[@inline] cbr_dw x =
  if x >= 0. then RD.(x *. x *. x) else RD.(x *. RU.(x *. x))

let[@inline] cbr_up x =
  if x >= 0. then RU.(x *. x *. x) else RU.(x *. RD.(x *. x))

let rec pow_IN_dw x n = (* x ∈ ℝ, n ≥ 0 *)
  if is_even n then RD.(pos_pow_IN 1. (x *. x) (n / 2))
  else if x >= 0. then RD.(pos_pow_IN x (x *. x) (n / 2))
  else RD.(x *. RU.(pos_pow_IN 1. (x *. x) (n / 2)))
and pow_i_dw x = function
  | 0 -> 1.
  | 1 -> x
  | 2 -> RD.(x *. x)
  | 3 -> cbr_dw x
  | 4 -> RD.(let x2 = x *. x in x2 *. x2)
  | n -> if n >= 0 then pow_IN_dw x n
         else (* Since the rounding has the same sign than xⁿ, we can
                 treat u ↦ 1/u as decreasing. *)
           RD.(1. /. pow_IN_up x (- n))
and pow_IN_up x n =
  if is_even n then RU.(pos_pow_IN 1. (x *. x) (n / 2))
  else if x >= 0. then RU.(pos_pow_IN x (x *. x) (n / 2))
  else RU.(x *. RD.(pos_pow_IN 1. (x *. x) (n / 2)))
and pow_i_up x = function
  | 0 -> 1.
  | 1 -> x
  | 2 -> RU.(x *. x)
  | 3 -> cbr_up x
  | 4 -> RU.(let x2 = x *. x in x2 *. x2)
  | n -> if n >= 0 then pow_IN_up x n else RU.(1. /. pow_IN_dw x (- n))

(* The [RoundDown] and [RoundUp] modules below depend on both the previous
   [RD] and [RU]. *)
module RoundDown = struct
  include RD

  let cbr = cbr_dw

  (* xⁿ for x ≤ 0 and n ≥ 0.  Useful for the interval extension. *)
  let[@inline] neg_pow_IN x = function
    | 0 -> 1.
    | 1 -> x
    | 2 -> x *. x
    | 3 -> x *. RU.(x *. x)
    | 4 -> let x2 = x *. x in x2 *. x2
    | n -> if is_even n then pos_pow_IN 1. (x *. x) (n / 2)
           else x *. RU.(pos_pow_IN 1. (x *. x) (n / 2))

  let pow_i = pow_i_dw
end

module RoundUp = struct
  include RU

  let cbr = cbr_up

  (* xⁿ for x ≤ 0 and n ≥ 0.  Useful for the interval extension. *)
  let[@inline] neg_pow_IN x = function
    | 0 -> 1.
    | 1 -> x
    | 2 -> x *. x
    | 3 -> x *. RD.(x *. x)
    | 4 -> let x2 = x *. x in x2 *. x2
    | n -> if is_even n then pos_pow_IN 1. (x *. x) (n / 2)
           else x *. RD.(pos_pow_IN 1. (x *. x) (n / 2))

  let pow_i = pow_i_up
end

module Low = RoundDown
module High = RoundUp

module type DIRECTED = sig
  type t
  val zero : t
  val one : t
  val pi: t
  val two_pi : t
  val half_pi : t
  val euler: t
  val float: int -> t
  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t
  val sqr : t -> t
  val cbr : t -> t
  val pow_i : t -> int -> t
  val sqrt : t -> t
end


type t = {low: float; high: float}

exception Division_by_zero
exception Domain_error of string

module I = struct
  type number = float
  type interval = t
  type t = interval
  (* Invariants (enforced by [I.v]:
     - -∞ ≤ low ≤ high ≤ +∞.  In particular, no bound is NaN.
     - [-∞,-∞] and [+∞,+∞] are not allowed. *)

  module U = Interval__U   (* Save original operators *)

  let zero = {low=0.; high=0.}
  let one = {low=1.; high=1.}
  let entire = {low = neg_infinity;  high = infinity}

  let pi = {low = RoundDown.pi; high = RoundUp.pi }
  let two_pi = {low = RoundDown.two_pi; high = RoundUp.two_pi }
  let half_pi = {low = RoundDown.half_pi; high = RoundUp.half_pi }
  let euler = {low = RoundDown.euler; high = RoundUp.euler }


  let v (a: float) (b: float) =
    if a < b (* ⇒ a, b not NaN; most frequent case *) then
      { low=a; high=b }
    else if a = b then
      if a = neg_infinity then
        invalid_arg "Interval.I.v: [-inf, -inf] is not allowed"
      else if a = infinity then
        invalid_arg "Interval.I.v: [+inf, +inf] is not allowed"
      else { low=a; high=b }
    else (* a > b or one of them is NaN *)
      invalid_arg("Interval.I.v: [" ^ string_of_float a ^ ", "
                  ^ string_of_float b ^ "] not allowed")

  let min_neg_float = -. Float.max_float

  let singleton (x: float) =
    if x < infinity then
      if x > neg_infinity then { low = x; high = x }
      else { low = neg_infinity; high = min_neg_float }
    else (* x = infinity or NaN *)
      if x = x then { low = Float.max_float; high = infinity }
      else
        let sx = string_of_float x in
        invalid_arg("Interval.I.v: [" ^ sx ^ ", " ^ sx ^ "] not allowed")

  let[@inline] inf i = i.low
  let[@inline] sup i = i.high
  let low i = i.low
  let high i = i.high

  let of_int n = {low = RoundDown.float n; high = RoundUp.float n}

  let to_string_fmt fmt i =     (* FIXME: rounding *)
    Printf.sprintf "[%(%f%), %(%f%)]" fmt i.low fmt i.high

  let to_string ?(fmt=("%g": _ format)) i = to_string_fmt fmt i

  let fmt fmt_float =
    let open CamlinternalFormatBasics in
    let to_string () i = to_string_fmt fmt_float i in
    let fmt = Custom(Custom_succ Custom_zero, to_string, End_of_format) in
    Format(fmt , "Inverval.t")

  let pr_fmt ?fmt:fmt_float ch i = (* FIXME: temp string unneeded *)
    output_string ch (to_string ?fmt:fmt_float i)

  let pp_fmt ?fmt:fmt_float f i = (* FIXME: temp string unneeded *)
    Format.fprintf f "%s" (to_string ?fmt:fmt_float i)

  module Precision = struct
    (* Global precision state.  [None] means: use as many digits as
       needed to print the number. *)
    let p = ref None

    let get () = !p

    let set = function
      | None -> p := None
      | Some p' as v -> if p' > 0 && p' <= 16 then p := v else p := None

    let fmt () =
      let open CamlinternalFormatBasics in
      match !p with
      | None -> None
      | Some p ->
         let conv = (Float_flag_, Float_g) in
         Some(Format(Float(conv, No_padding, Lit_precision p, End_of_format),
                     "Interval.t"))
  end

  let pr ch i = pr_fmt ?fmt:(Precision.fmt()) ch i
  let pp f i = pp_fmt ?fmt:(Precision.fmt()) f i

  let compare_f {low = a; high = b} x =
    if b < x then 1 else if a <= x then 0 else -1

  let belong x {low; high} = low <= x && x <= high

  let is_bounded {low; high} =
    neg_infinity < low && high < infinity

  let is_entire {low; high} =
    neg_infinity = low && high = infinity

  let equal {low = a; high = b} {low = c; high = d} =
    a = c && b = d

  let subset {low = a; high = b} {low = c; high = d} =
    (* No empty intervals. *)
    c <= a && b <= d

  let less {low = a; high = b} {low = c; high = d} =
    a <= c && b <= d

  let precedes x y = x.high <= y.low (* intervals are not empty *)

  let interior {low = a; high = b} {low = c; high = d} =
    (* Intervals are not empty *)
    (c < a || (c = neg_infinity && a = neg_infinity))
    && (b < d || (b = infinity && d = infinity))

  let strict_less {low = a; high = b} {low = c; high = d} =
    (* Intervals are not empty *)
    (a < c || (a = neg_infinity && c = neg_infinity))
    && (b < d || (b = infinity && d = infinity))

  let strict_precedes x y = x.high < y.low (* intervals not empty *)

  let disjoint {low = a; high = b} {low = c; high = d} =
    (* Intervals are not empty *)
    b < c || d < a

  let width x =
    { low = RoundDown.(x.high -. x.low);  high = RoundUp.(x.high -. x.low) }

  let width_dw x = RoundDown.(x.high -. x.low)
  let width_up x = RoundUp.(x.high -. x.low)
  let width_low = width_dw
  let width_high = width_up

  let diam = width_high
  let size = width
  let size_low = width_low
  let size_high = width_high

  let[@inline] dist_up x y =
    fmax (RoundUp.dist x.low y.low) (RoundUp.dist x.high y.high)

  let dist x y =
    { low = fmax (RoundDown.dist x.low y.low) (RoundDown.dist x.high y.high);
      high = dist_up x y }

  let mag x = fmax (abs_float x.low) (abs_float x.high)

  let mig x = if x.low >= 0. then x.low
              else if x.high <= 0. then -. x.high
              else (* x.low < 0 < x.high *) 0.

  let abs ({low = a; high = b} as x) =
    if 0. <= a then x
    else if b <= 0. then {low = -.b; high = -.a}
    else {low = 0.; high = fmax (-.a) b}

  let sgn {low = a; high = b} =
    {low = float (compare a 0.); high = float (compare b 0.)}

  let truncate x =
    {low = floor x.low; high = ceil x.high}

  let hull x y = {low = fmin x.low y.low; high = fmax x.high y.high}

  let inter_exn {low = a; high = b} {low = c; high = d} =
    let low = fmax a c in
    let high = fmin b d in
    if low <= high then {low; high}
    else raise(Domain_error "I.inter_exn")

  let inter {low = a; high = b} {low = c; high = d} =
    let low = fmax a c in
    let high = fmin b d in
    if low <= high then Some {low; high} else None

  let max x y = {low = fmax x.low y.low; high = fmax x.high y.high}

  let min x y = {low = fmin x.low y.low; high = fmin x.high y.high}

  let ( + ) {low = a; high = b} {low = c; high = d} =
    { low = RoundDown.(a +. c);  high = RoundUp.(b +. d) }

  let ( - ) {low = a; high = b} {low = c; high = d} =
    { low = RoundDown.(a -. d);  high = RoundUp.(b -. c) }

  let ( +. ) {low = a; high = b} x =
    { low = RoundDown.(a +. x);  high = RoundUp.(b +. x) }

  let ( +: ) x {low = a; high = b} =
    { low = RoundDown.(a +. x);  high = RoundUp.(b +. x) }

  let ( -. ) {low = a; high = b} x =
    { low = RoundDown.(a -. x);  high = RoundUp.(b -. x) }

  let ( -: ) x {low = c; high = d} =
    { low = RoundDown.(x -. d);  high = RoundUp.(x -. c) }

  let ( ~- ) {low = a; high = b} = {low = -.b; high = -.a}

  let ( * ) {low = a; high = b} {low = c; high = d} =
    let sa = compare a 0. and sb = compare b 0. in
    let sc = compare c 0. and sd = compare d 0. in
    if (sa = 0 && sb = 0) || (sc = 0 && sd = 0) then {low = 0.; high = 0.}
    else if sb <= 0 then
      if sd <= 0 then {low = RoundDown.(b *. d); high = RoundUp.(a *. c)}
      else if 0 <= sc then {low = RoundDown.(a *. d); high = RoundUp.(b *. c)}
      else {low = RoundDown.(a *. d); high = RoundUp.(a *. c)}
    else if 0 <= sa then
      if sd <= 0 then {low = RoundDown.(b *. c); high = RoundUp.(a *. d)}
      else if 0 <= sc then {low = RoundDown.(a *. c); high = RoundUp.(b *. d)}
      else {low = RoundDown.(b *. c); high = RoundUp.(b *. d)}
    else if 0 <= sc then {low = RoundDown.(a *. d); high = RoundUp.(b *. d)}
    else if sd <= 0 then {low = RoundDown.(b *. c); high = RoundUp.(a *. c)}
    else
      { low = fmin RoundDown.(a *. d) RoundDown.(b *. c);
        high = fmax RoundUp.(a *. c) RoundUp.(b *. d) }

  let ( *. ) y {low = a; high = b} =
    let sy = compare y 0. in
    if sy = 0 then {low = 0.; high = 0.}
    else if sy < 0 then {low = RoundDown.(b *. y); high = RoundUp.(a *. y)}
    else {low = RoundDown.(a *. y); high = RoundUp.(b *. y)}

  let ( *: ) a y = y *. a

  let ( / ) {low = a; high = b} {low = c; high = d} =
    let sc = compare c 0. and sd = compare d 0. in
    if sd = 0 then
      if sc = 0 then raise Division_by_zero
      else if b <= 0. then
        {low = RoundDown.(b /. c); high = if a = 0. then 0. else infinity}
      else if 0. <= a then {low = neg_infinity; high = RoundUp.(a /. c)}
      else {low = neg_infinity; high = infinity}
    else if sd < 0 then
      { low = if b <= 0. then RoundDown.(b /. c) else RoundDown.(b /. d);
        high = if 0. <= a then RoundUp.(a /. c) else RoundUp.(a /. d) }
    else if sc = 0 then
      if b <= 0. then
        {low = if a = 0. then 0. else neg_infinity; high = RoundUp.(b /. d)}
      else if 0. <= a then {low = RoundDown.(a /. d); high = infinity}
      else {low = neg_infinity; high = infinity}
    else if 0 < sc then
      { low = if a <= 0. then RoundDown.(a /. c) else RoundDown.(a /. d);
        high = if b <= 0. then RoundUp.(b /. d) else RoundUp.(b /. c) }
    else if a = 0. && b = 0. then {low = 0.; high = 0.}
    else {low = neg_infinity; high = infinity}

  let ( /. ) {low = a; high = b} y =
    let sy = compare y 0. in
    if sy = 0 then raise Division_by_zero
    else if 0 < sy then {low = RoundDown.(a /. y); high = RoundUp.(b /. y)}
    else {low = RoundDown.(b /. y); high = RoundUp.(a /. y)}

  let ( /: ) x {low = a; high = b} =
    let sx = compare x 0. and sa = compare a 0. and sb = compare b 0. in
    if sx = 0 then
      if sa = 0 && sb = 0 then raise Division_by_zero
      else {low = 0.; high = 0.}
    else if 0 < sa || sb < 0 then
      if 0 < sx then {low = RoundDown.(x /. b); high = RoundUp.(x /. a)}
      else {low = RoundDown.(x /. a); high = RoundUp.(x /. b)}
    else if sa = 0 then
      if sb = 0 then raise Division_by_zero
      else if 0 <= sx then {low = RoundDown.(x /. b); high = infinity}
      else {low = neg_infinity; high = RoundUp.(x /. b)}
    else if sb = 0 then
      if sx = 0 then {low = 0.; high = 0.}
      else if 0 <= sx then {low = neg_infinity; high = RoundUp.(x /. a)}
      else {low = RoundDown.(x /. a); high = infinity}
    else {low = neg_infinity; high = infinity}

  let inv {low = a; high = b} =
    let sa = compare a 0. and sb = compare b 0. in
    if sa = 0 then
      if sb = 0 then raise Division_by_zero
      else {low = RoundDown.(1. /. b); high = infinity}
    else if 0 < sa || sb < 0 then {low = RoundDown.(1. /. b);
                                  high = RoundUp.(1. /. a)}
    else if sb = 0 then {low = neg_infinity; high = RoundUp.(1. /. a)}
    else {low =  neg_infinity; high = infinity}

  type 'a one_or_two = One of 'a | Two of 'a * 'a

  let invx {low = a; high = b} =
    let sa = compare a 0. and sb = compare b 0. in
    if sa = 0 then
      if sb = 0 then raise Division_by_zero
      else One {low = RoundDown.(1. /. b); high = infinity}
    else if 0 < sa || sb < 0 then One {low = RoundDown.(1. /. b);
                                      high = RoundUp.(1. /. a)}
    else if sb = 0 then One {low = neg_infinity; high = RoundUp.(1. /. a)}
    else Two({low = neg_infinity;  high = RoundUp.(1. /. a) },
             {low = RoundDown.(1. /. b);  high = infinity})

  let cancelminus x y =
    (* Intervals here cannot be empty. *)
    if is_bounded x && is_bounded y then
      let low = RoundDown.(x.low -. y.low) in
      let high = RoundUp.(x.high -. y.high) in
      if low <= high (* thus not NaN *) then {low; high}
      else entire
    else entire

  let cancelplus x y = (* = cancelminus x (-y) *)
    if is_bounded x && is_bounded y then
      let low = RoundDown.(x.low +. y.high) in
      let high = RoundUp.(x.high +. y.low) in
      if low <= high (* thus not NaN *) then {low; high}
      else entire
    else entire

  let sqr {low = a; high = b} =
    if a >= 0. then {low = RoundDown.(a *. a); high = RoundUp.(b *. b)}
    else (* a < 0; a is not NaN *)
      if b >= 0. then {low = 0.; high = fmax RoundUp.(a *. a) RoundUp.(b *. b)}
      else {low = RoundDown.(b *. b); high = RoundUp.(a *. a)}

  let cbr {low = a; high = b} =
    {low = RoundDown.cbr a; high = RoundUp.cbr b}

  let pow_IN x = function
    | 0 -> one
    | 1 -> x
    | 2 -> sqr x
    | 3 -> cbr x
    | n  -> (* n ≥ 0 assumed *)
       if is_even n then
         if x.low >= 0. then
           {low = RoundDown.pos_pow_IN 1. x.low n;
            high = RoundUp.pos_pow_IN 1. x.high n}
         else if x.high > 0. then (* x.low < 0 < x.high *)
           {low = 0.;  high = fmax RoundUp.(neg_pow_IN x.low n)
                                RoundUp.(pos_pow_IN 1. x.high n)}
         else (* x.low ≤ x.high ≤ 0 *)
           {low = RoundDown.neg_pow_IN x.high n;
            high = RoundUp.neg_pow_IN x.low n}
       else (* x ↦ xⁿ is increasing. *)
         {low = RoundDown.pow_i x.low n;  high = RoundUp.pow_i x.high n}

  let ( ** ) x n =
    if n >= 0 then pow_IN x n else inv(pow_IN x U.(- n))

  let sqrt {low = a; high = b} =
    if b < 0. then raise(Domain_error "sqrt")
    else {low = if a < 0. then 0. else RoundDown.sqrt a; high = RoundUp.sqrt b}

  (* Infix aliases *)
  let ( = ) = equal
  let ( <= ) = less
  let ( < ) = strict_less
  let ( >= ) x y = less y x
  let ( > ) x y = strict_less y x
end


external set_round_dw: unit -> unit = "ocaml_set_low" [@@noalloc]
external set_round_up: unit -> unit = "ocaml_set_high" [@@noalloc]
external set_round_nearest: unit -> unit = "ocaml_set_nearest" [@@noalloc]
