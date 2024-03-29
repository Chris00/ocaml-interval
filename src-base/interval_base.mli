(*
    Copyright 2011 Jean-Marc Alliot / Jean-Baptiste Gotteland
    Copyright 2018 Christophe Troestler

    This file is part of the OCaml interval library.

    The OCaml interval library is free software:
    you can redistribute it and/or modify it under the terms of
    the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The OCaml interval library is distributed in the hope that it will be
    useful,but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with the OCaml interval library.
    If not, see <http://www.gnu.org/licenses/>.
*)

(** Base module for interval arithmetic.

   @version %%VERSION%% *)

(** Basic signature for interval arithmetic packages. *)
module type T = sig
  type number
  (** Numbers type on which intervals are defined. *)

  type t
  (** The type of intervals. *)

  val zero : t
  (** Neutral element for addition. *)

  val one : t
  (** Neutral element for multiplication. *)

  val pi: t
  (** π with bounds properly rounded. *)

  val two_pi : t
  (** 2π with bounds properly rounded. *)

  val half_pi : t
  (** π/2 with bounds properly rounded. *)

  val euler: t
  (** Euler's constant with bounds properly rounded.
      @since 1.6 *)

  val entire : t
  (** The entire set of {!number}s.
     @since 1.5 *)

  val v : number -> number -> t
  (** [v a b] returns the interval \[[a], [b]\].  BEWARE that, unless you take
     care, if you use [v a b] with literal values for [a] and/or [b],
     the resulting interval may not contain these values because the
     compiler will round them to binary numbers before passing them to
     [v].

     @raise Invalid_argument if the interval \[[a], [b]\] is equal to
     \[-∞,-∞\] or \[+∞,+∞\] or one of the bounds is NaN. *)

  val inf : t -> number
  (** [inf t] returns the lower bound of the interval.
      @since 1.6 *)

  val sup : t -> number
  (** [sup t] returns the higher bound of the interval.
      @since 1.6 *)

  val singleton : number -> t
  (** [singleton x] returns the same as [{!v} x x] except that checks
     on [x] are only performed once and infinite values of [x] work
     according to the specification of intervals.
     @since 1.6 *)

  val of_int : int -> t
  (** Returns the interval containing the conversion of an integer to
     the number type. *)

  val to_string : ?fmt: (number -> 'b, 'a, 'b) format -> t -> string
  (** [to_string i] return a string representation of the interval [i].
      @param fmt is the format used to print the two bounds of [i].
                 Default: ["%g"] for float {!number}s. *)

  val pr : out_channel -> t -> unit
  (** Print the interval to the channel.  To be used with [Printf]
     format "%a". *)

  val pr_fmt : ?fmt: (number -> 'b, 'a, 'b) format ->
               out_channel -> t -> unit
  (** Same as {!pr} but enables to choose the format. *)

  val pp : Format.formatter -> t -> unit
  (** Print the interval to the formatter.  To be used with [Format]
     format "%a". *)

  val pp_fmt : ?fmt: (number -> 'b, 'a, 'b) format ->
               Format.formatter -> t -> unit
  (** Same as {!pp} but enables to choose the format. *)

  val fmt : (number -> 'b, 'a, 'b) format -> (t -> 'c, 'd, 'e, 'c) format4
  (** [fmt number_fmt] returns a format to print intervals where each
     component is printed with [number_fmt].

     Example: [Printf.printf ("%s = " ^^ fmt "%.10f" ^^ "\n") name i]. *)

  (** {2 Boolean functions} *)

  val compare_f: t -> number -> int
  (** [compare_f a x] returns
      - [1] if [sup(a) < x],
      - [0] if [inf(a)] ≤ [x] ≤ [sup(a)], i.e., if [x] ∈ [a], and
      - [-1] if [x < inf(a)].  *)

  val belong : number -> t -> bool
  (** [belong x a] returns whether [x] ∈ [a]. *)

  val is_singleton : t -> bool
  (** [is_singleton x] says whether [x] is a ingleton i.e., the two
     bounds of [x] are equal.
     @since 1.6 *)

  val is_bounded : t -> bool
  (** [is_bounded x] says whether the interval is bounded, i.e.,
      -∞ < [inf(x)] and [sup(x)] < ∞.
      @since 1.5 *)

  val is_entire : t -> bool
  (** [is_entire x] says whether [x] is the {!entire} interval.
      @since 1.5 *)

  val equal : t -> t -> bool
  (** [equal a b] says whether the two intervals are the same.
      @since 1.5 *)

  val ( = ) : t -> t -> bool
  (** Synonym for {!equal}.
      @since 1.5 *)

  val subset : t -> t -> bool
  (** [subset x y] returns true iff [x] ⊆ [y].
      @since 1.5 *)

  val ( <= ) : t -> t -> bool
  (** [x <= y] says whether [x] is weakly less than [y] i.e.,
      ∀ξ ∈ [x], ∃η ∈ [y], ξ ≤ η and ∀η ∈ [y], ∃ξ ∈ [x], ξ ≤ η.
      @since 1.5 *)

  val ( >= ) : t -> t -> bool
  (** [x >= y] says whether [x] is weakly greater than [y] i.e.,
      ∀ξ ∈ [x], ∃η ∈ [y], ξ ≥ η and ∀η ∈ [y], ∃ξ ∈ [x], ξ ≥ η.
      @since 1.5 *)

  val precedes : t -> t -> bool
  (** [precedes x y] returns true iff [x] is to the left but may touch [y].
      @since 1.5 *)

  val interior : t -> t -> bool
  (** [interior x y] returns true if [x] is interior to [y] in the
     topological sense.  For example [interior entire entire] is [true].
     @since 1.5 *)

  val ( < ) : t -> t -> bool
  (** [x < y] says whether [x] is strictly weakly less than [y] i.e.,
      ∀ξ ∈ [x], ∃η ∈ [y], ξ < η and ∀η ∈ [y], ∃ξ ∈ [x], ξ < η.
      @since 1.5 *)

  val ( > ) : t -> t -> bool
  (** [x > y] iff [y < x].
      @since 1.5 *)

  val strict_precedes : t -> t -> bool
  (** [strict_precedes x y] returns true iff [x] is to the left and
     does not touch [y].
     @since 1.5 *)

  val disjoint : t -> t -> bool
  (** [disjoint x y] returns true iff [x] ∩ [y] = ∅.
      @since 1.5 *)


  (** {2 Operations} *)

  val width: t -> t
  (** [size a] returns an interval containing the true width of the
     interval [sup a - inf a]. *)

  val width_up : t -> number
  (** [width_up a] returns the width of the interval [sup a - inf a]
     rounded up. *)

  val width_dw : t -> number
  (** [width_dw a] returns the width of the interval [sup a - inf a]
     rounded down. *)

  val diam : t -> number
  (** Alias for [width_up] (page 64 of IEEE1788). *)

  val dist : t -> t -> t
  (** [dist x y] is the Hausdorff distance between [x] and [y].
      It is equal to max\{ |[inf x] - [inf y]|, |[sup x] - [sup y]| \}.
      @since 1.6 *)

  val dist_up : t -> t -> number
  (** [dist_up x y] is the Hausdorff distance between [x] and [y],
     rounded up.  (This satisfies the triangular inequality for a
     rounded up [+.].) *)

  val mag : t -> number
  (** [mag x] returns the magnitude of [x], that is sup\{ |x| : x ∈ [x] \}. *)

  val mig : t -> number
  (** [mig x] returns the mignitude of [x], that is inf\{ |x| : x ∈ [x] \}. *)

  val mid : t -> number
  (** [mid x] returns a finite number belonging to [x] which is close
     to the midpoint of [x].  If {!is_entire} [x], zero is returned. *)

  val sgn: t -> t
  (** [sgn a] returns the sign of each bound, e.g., for floats
      \[[float (compare (inf a) 0.)], [float (compare (sup a) 0.)]\]. *)

  val truncate: t -> t
  (** [truncate a] returns the integer interval containing [a], that is
      \[[floor(inf a)], [ceil(sup a)]\]. *)

  val floor: t -> t
  (** [floor a] returns the floor of [a], that is
      \[[floor(inf a)], [floor(sup a)]\]. *)

  val ceil: t -> t
  (** [ceil a] returns the ceil of [a], that is
      \[[ceil(inf a)], [ceil(sup a)]\]. *)

  val abs: t -> t
  (** [abs a] returns the absolute value of the interval, that is
      - [a] if [inf a] ≥ [0.],
      - [~- a] if [sup a] ≤ [0.], and
      - \[0, [max (- inf a) (sup a)]\] otherwise. *)

  val hull: t -> t -> t
  (** [hull a b] returns the smallest interval containing [a] and [b], that is
      \[[min (inf a) (inf b)], [max (sup a) (sup b)]\]. *)

  val inter_exn : t -> t -> t
  (** [inter_exn x y] returns the intersection of [x] and [y].
      @raise Domain_error if the intersection is empty.
      @since 1.5 *)

  val inter : t -> t -> t option
  (** [inter_exn x y] returns [Some z] where [z] is the intersection
     of [x] and [y] if it is not empty and [None] if the intersection
     is empty.
     @since 1.5 *)

  val max: t -> t -> t
  (** [max a b] returns the "maximum" of the intervals [a] and [b], that is
      \[[max (inf a) (inf b)], [max (sup a) (sup b)]\]. *)

  val min: t -> t -> t
  (** [min a b] returns the "minimum" of the intervals [a] and [b], that is
      \[[min (inf a) (inf b)], [min (sup a) (sup b)]\]. *)

  val ( + ) : t -> t -> t
  (** [a + b] returns \[[inf a +. inf b], [sup a +. sup b]\]
     properly rounded. *)

  val ( +. ): t -> number -> t
  (** [a +. x] returns \[[inf a +. x], [sup a +. x]\]
      properly rounded. *)

  val ( +: ): number -> t -> t
  (** [x +: a] returns \[[a +. inf a], [x +. sup a]\]
      properly rounded. *)

  val ( - ): t -> t -> t
  (** [a - b] returns \[[inf a -. sup b], [sup a -. inf b]\]
      properly rounded. *)

  val ( -. ): t -> number -> t
  (** [a -. x] returns \[[inf a -. x],  [sup a -. x]\]
      properly rounded. *)

  val ( -: ): number -> t -> t
  (** [x -: a] returns \[[x -. sup a], [x -. inf a]\]
      properly rounded. *)

  val ( ~- ): t -> t
  (** [~- a] is the unary negation, it returns \[[-sup a], [-inf a]\]. *)

  val ( * ): t -> t -> t
  (** [a * b] multiplies [a] by [b] according to interval arithmetic
     and returns the proper result.  If [a=zero] or [b=zero] then
     {!zero} is returned. *)

  val ( *. ): number -> t -> t
  (** [x *. a] returns the multiplication of [a] by [x] according to
     interval arithmetic.  If [x=0.] then {!zero} is returned.

     Note that the scalar comes first in this “dotted operator” (as
     opposed to other ones) because it is customary in mathematics to
     place scalars first in products and last in sums.
     Example: [3. *. x**2 + 2. *. x +. 1.] *)

  val ( *: ): t -> number -> t
  (** [a *. x] multiplies [a] by [x] according to interval arithmetic
     and returns the proper result.  If [x=0.] then {!zero} is returned. *)

  val ( / ): t -> t -> t
  (** [a / b] divides the first interval by the second according to
     interval arithmetic and returns the proper result.
     Raise [Interval.Division_by_zero] if [b=]{!zero}. *)

  val ( /. ): t -> number -> t
  (** [a /. x] divides [a] by [x] according to interval arithmetic and
     returns the proper result.
     Raise [Interval.Division_by_zero] if [x=0.0]. *)

  val ( /: ): number -> t -> t
  (** [x /: a] divides [x] by [a] according to interval arithmetic and
     returns the result.
     Raise [Interval.Division_by_zero] if [a=]{!zero}. *)

  val inv: t -> t
  (** [inv a] returns [1. /: a] but is more efficient.
      Raise [Interval.Division_by_zero] if [a=]{!zero}. *)

  type 'a one_or_two = One of 'a | Two of 'a * 'a

  val invx : t -> t one_or_two
  (** [invx a] is the extended division.  When 0 ∉ [a], the result is
     [One(inv a)].  If 0 ∈ [a], then the two natural intervals
     (properly rounded) [Two](\[-∞, 1/(inf a)\], \[1/(sup a), +∞\]) are
     returned.
     Raise [Interval.Division_by_zero] if [a=]{!zero}. *)

  val cancelminus : t -> t -> t
  (** [cancelminus x y] returns the tightest interval [z] such that
     [x] ⊆ [z] + [y].  If no such [z] exists, it returns [entire].
     @since 1.5 *)

  val cancelplus : t -> t -> t
  (** [cancelplus x y] returns the tightest interval [z] such that
     [x] ⊆ [z] - [y].  If no such [z] exists, it returns [entire].
     @since 1.5 *)

  val ( ** ): t -> int -> t
  (** [a**n] returns interval [a] raised to [n]th power according
     to interval arithmetic.  If [n=0] then {!one} is returned.

     @raise Domain_error if [n < 0] and [a=]{!zero}. *)

  val sqrt : t -> t
  (** [sqrt x] returns an enclosure for the square root of [x]. *)

  val hypot : t -> t -> t
  (** [hypot x y] returns an enclosure of [sqrt(x *. x + y *. y)]. *)
end


(** {2 Intervals with float endpoints} *)

(** The interval type. Be careful however when creating intervals. For
   example, the following code: [let a = {low=1./.3.; high=1./.3.}]
   creates an interval which does NOT contain the mathematical object
   1/3.

   If you want to create an interval representing 1/3, you have to
   write [let a = I.(inv(v 3. 3.))] because rounding will then be
   properly handled by {!I.inv} and the resulting interval will indeed
   contain the exact value of 1/3. *)
type interval = {
    low: float; (** lower bound, possibly = -∞ *)
    high: float (** higher bound, possibly = +∞ *)
  }

exception Division_by_zero
(** Exception raised when a division by 0 occurs. *)

exception Domain_error of string [@@warn_on_literal_pattern]
(** Exception raised when an interval is completely outside the domain
   of a function.  The string is the name of the function and is meant
   to help when running code in the REPL (aka toploop). *)

(** Interval operations.  Locally open this module — using
   e.g. [I.(...)] — to redefine classical arithmetic operators for
   interval arithmetic. *)
module I : sig
  include T with type number = float and type t = interval

  val low : t -> number   [@@deprecated "Use I.inf"]
  (** [low t] returns the lower bound of the interval. *)

  val high : t -> number  [@@deprecated "Use I.sup"]
  (** [high t] returns the higher bound of the interval. *)

  val width_high : t -> number [@@deprecated "Use I.width_up"]
  val width_low : t -> number  [@@deprecated "Use I.width_dw"]

  (** Global precision for the functions {!I.pr} and {!I.pp}. *)
  module Precision : sig
    val set : int option -> unit
    (** Set the number of decimals used by {!pr} and {!pp}.  If
       [None], use as many digits as needed to accurately print the
       interval. *)

    val get : unit -> int option
    (** Return the decimal precision used by {!pp}, if any. *)
  end

  val size : t -> t           [@@deprecated "Use I.width"]
  val size_high : t -> number [@@deprecated "Use I.width_up"]
  val size_low : t -> number  [@@deprecated "Use I.width_dw"]

  (** {2 Usual arithmetic operators} *)

  (** Module undoing the redeclaration of usual infix operators [+],
     [+.], etc. in case it is needed locally, while this module is
     open.

     Example: [I.(x + sin(of_int U.(n + 1)))]. *)
  module U : sig
    (** Restore standard integer and floating point operators. *)

    external ( ~- ) : int -> int = "%negint"
    external ( ~+ ) : int -> int = "%identity"
    external ( + ) : int -> int -> int = "%addint"
    external ( - ) : int -> int -> int = "%subint"
    external ( * ) : int -> int -> int = "%mulint"
    external ( / ) : int -> int -> int = "%divint"

    external ( ~-. ) : float -> float = "%negfloat"
    external ( ~+. ) : float -> float = "%identity"
    external ( +. ) : float -> float -> float = "%addfloat"
    external ( -. ) : float -> float -> float = "%subfloat"
    external ( *. ) : float -> float -> float = "%mulfloat"
    external ( /. ) : float -> float -> float = "%divfloat"
    external ( ** ) : float -> float -> float = "caml_power_float" "pow"
                                                  [@@unboxed] [@@noalloc]
    external sqrt : float -> float = "caml_sqrt_float" "sqrt"
                                    [@@unboxed] [@@noalloc]

    external ( = ) : 'a -> 'a -> bool = "%equal"
    external ( <> ) : 'a -> 'a -> bool = "%notequal"
    external ( < ) : 'a -> 'a -> bool = "%lessthan"
    external ( > ) : 'a -> 'a -> bool = "%greaterthan"
    external ( <= ) : 'a -> 'a -> bool = "%lessequal"
    external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
  end
end

(** {2 Directed rounding} *)

(** Interface for up and down roundings. *)
module type DIRECTED = sig
  type t        (** Type of numbers. *)

  val zero : t  (** The neutral element for addition. *)

  val one : t   (** The neutral element for multiplication. *)

  val pi: t
  (** Upper/lower bound on π. *)

  val two_pi : t
  (** Upper/lower bound on 2π. *)

  val half_pi : t
  (** Upper/lower bound on π/2. *)

  val euler: t
  (** Upper/lower bound on Euler's constant. *)

  val float: int -> t
  (** When [t = float], the float function is exact on 32 bits machine
     but not on 64 bits machine with ints larger than 53 bits. *)

  val dist : t -> t -> t
  (** [dist x y] return the distance between [x] and [y] (i.e., |x-y|)
     rounded up or down according to the module.
     @since 1.6 *)

  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t

  val sqr : t -> t
  (** [sqr x] returns an upper/lower bound on [x]². *)

  val cbr : t -> t
  (** [cbr x] returns an upper/lower bound on [x]³. *)

  val pow_i : t -> int -> t
  (** [pow_i x n] return a upper/lower bound on [x]ⁿ. *)

  val sqrt : t -> t
  (** [sqrt x] return the square root of [x] rounded up/down. *)

  val hypot : t -> t -> t
  (** [hypot x y] returns [sqrt(x *. x + y *. y)] rounded up/down.
      Returns NaN if any of the arguments is NaN. *)
end


(** Functions rounding down their results. *)
module RoundDown : sig
  include DIRECTED with type t = float

  (** Locally open to restore standard integer and floating point
     operators. *)
  module U = I.U
end

(** Functions rounding up their results. *)
module RoundUp : sig
  include DIRECTED with type t = float

  (** Locally open to restore standard integer and floating point
     operators. *)
  module U = I.U
end


(** {2 Changing the rounding mode (DANGEROUS)} *)

(** Below, we have functions for changing the rounding mode.  The
   default mode for rounding is NEAREST.

   BE VERY CAREFUL: using these functions unwisely can ruin all your
   computations. Remember also that on 64 bits machine these functions
   won't change the behaviour of the SSE instructions.

   When setting the rounding mode to UPWARD or DOWNWARD, it is better
   to set it immediately back to NEAREST. However we have no guarantee
   on how the compiler will reorder the instructions generated.  It is
   ALWAYS better to write:
   {[
   let a = set_round_up(); let res = 1./.3. in
   set_round_nearest (); res;; ]}

   The above code will NOT work on linux-x64 where many floating point
   functions are implemented using SSE instructions.  These three
   functions should only be used when there is no other solution, and
   you really know what tou are doing, and this should never happen.
   Please use the regular functions of the fpu module for
   computations.  For example prefer:
   {[ let a = RoundUp.(1. /. 3.)  ]}

   PS: The Interval module and the fpu module functions correctly set and
   restore the rounding mode for all interval computations, so you
   don't really need these functions.

   PPS: Please, don't use them...  *)

val set_round_dw: unit -> unit
(** Sets the rounding mod to DOWNWARD (towards minus infinity) *)

val set_round_up: unit -> unit
(** Sets the rounding mod to UPWARD (towards infinity) *)

val set_round_nearest: unit -> unit
(** Sets the rounding mod to NEAREST (default mode) *)


(**/**)

module Low = RoundDown [@@deprecated "Use Interval.RoundDown"]
module High = RoundUp [@@deprecated "Use Interval.RoundUp"]

type t = interval [@@deprecated "Use Interval_base.I.t instead"]
