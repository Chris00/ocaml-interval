(*
    Copyright 2011 Jean-Marc Alliot / Jean-Baptiste Gotteland

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


(** Interval library in OCaml.  ONLY FOR INTEL PROCESSORS.

   All operations use correct rounding.

   It is {i not} recommended to open this module but to alias it at
   the beginning of your code and then open it locally:
{[module I = Interval

let x = I.(v 0.5 1. + sin(v 3. 3.125))
]}
   When the module [I] is open, the integer operators ([+], [-],...)
   and the floating point ones ([+.], [-.],...) are redefined.  If, in
   the middle of an expression, you need to use the usual operators,
   locally open the module {!U} as in [Interval.(x /. U.(float(n + 1)))].

   The names symbols for infix operators have been chosen to try to
   make the standard cases short and the overall expression readable.
   The rationale is as follows.
   - The integer operators [+], [-], [*], [/], [~-] and [~+]
     act on intervals;
   - The float operators [+.], [-.], [/.] take an interval to
     the left and a float to the right, but [*.] does the opposite.
     This is to match the standard presentation of polynomials.
     Example: [Interval.(3. *. x**2 + 2. *. x +. 4.)].
   - New operators [+:], [-:], [*:] and [/:] are as in the previous
     point bu with the interval and float swapped.  Note that [+:] and
     [*:] are not really needed because you can always swap arguments.
   - For exponentiation, [**] has been chosen for integer exponent
     because they are the more frequent, [**.] when the exponent is a
     float, [**:] when the base is a float and [***] for exponentiation
     of two intervals.

   You do not have to worry about remembering these rules.  The type
   system will enforce them.

   It is not mandatory, but still wise, to read the documentation of
   the {!Fpu} module.

   This library has been mainly designed to be used in a branch and
   bound optimization algorithm. So, some choices have been made:

   - NaN is never used. We either extend functions by pseudo
     continuity or raise exceptions. For example, [{low=2.;high=3.} /
     {low=0.;high=2.}] returns [{low=1.;high=Inf}], while
     [{low=2.;high=3.} / {low=0.;high=0.}] or [{low=0.;high=0.} /
     {low=0.;high=0.}] raise a failure.
   - Intervals \[+Inf,+Inf\] or \[-Inf,-Inf\] are never used and never
     returned.
   - When using a float in the following operations, it must never be
     equal to +Inf or -Inf or Nan.
   - Functions such as [log], [sqrt], [acos] or [asin] are restricted
     to their definition domain but raise an exception rather than
     returning an empty interval: for example [sqrt {low=-4;high=4}]
     returns [{low=0;high=2}] while [sqrt {low=-4;high=-2}] will raise
     an exception.

   Another design choice was to have non mutable elements in interval
   structure, and to maintain an "ordinary" syntax for operations,
   such as [let a = b+$c in ...] thus mapping interval computation
   formula on airthmetic formula. We could have instead chosen to have
   mutable elements, and to write for example ([Interval.add a b c])
   to perform “a = b + c”.  The first choice is, to our point of view,
   more elegant and easier to use.  The second is more efficient,
   especially when computing functions with many temporary results,
   which force the GC to create and destroy lot of intervals when
   using the implementation we chose. Nothing's perfect.

   An older deprecated interface is still available as the module
   {!Interval.Deprecated}.

   The library is implemented in x87 assembly mode and is quite
   efficient ({{:#perf}see below}).  *)


(** The interval type. Be careful however when creating intervals. For
   example, the following code: [let a = \{low=1./.3.;high=1./.3.\}]
   creates an interval which does NOT contain the mathematical object
   1/3.

   If you want to create an interval representing 1/3, you have to
   write [let a = 1. /.$ \{low=3.0;high=3.0\}] because rounding will
   then be properly set *)
type t = {
    low: float; (** low bound, possibly = -∞ *)
    high: float (** high bound, possibly = +∞ *)
  }

val zero : t
(** Neutral element for addition. *)

val one : t
(** Neutral element for multiplication. *)

val pi: t
(** π with bounds properly rounded. *)

val two_pi : t
(** 2π with bounds properly rounded. *)

val e: t
(** [e] (Euler's constant) with bounds properly rounded. *)

val v : float -> float -> t
(** [v a b] returns [{low=a; high=b}].  BEWARE that, unless you take
   care, if you use [v a b] with litteral values for [a] and/or [b],
   the resulting interval may not contain these values because the
   compiler will round them to binary numbers before passing them to [v]. *)

val of_int : int -> t
(** Returns the interval containing the float conversion of an integer. *)

val to_string : ?fmt: (float -> 'b, 'a, 'b) format -> t -> string
(** [to_string i] return a string representation of the interval [i].
    @param fmt is the format used to print the two bounds of [i].
               Default: ["%g"]. *)

val pr : out_channel -> t -> unit
(** Print the interval to the channel.  To be used with [Printf]
   format "%a". *)

val pp : Format.formatter -> t -> unit
(** Print the interval to the formatter.  To be used with [Format]
   format "%a". *)

val fmt : (float -> 'b, 'a, 'b) format -> (t -> 'c, 'd, 'e, 'c) format4
(** [fmt float_fmt] returns a format to print intervals where each
   component is printed with [float_fmt].

   Example: [Printf.printf ("%s = " ^^ fmt "%.10f" ^^ "\n") name i]. *)

val compare_f: t -> float -> int
(** [compare_f a x] returns
    - [1] if [a.high < x],
    - [0] if [a.low <= x <= a.high], i.e., if [x] ∈ [a], and
    - [-1] if [x < a.low].  *)

val size: t -> float
(** [size a] returns the length of the interval [a.high - a.low]. *)

val sgn: t -> t
(** [sgn a] returns [{low=float (compare a.low 0.);
    high=float (compare a.high 0.)}]. *)

val truncate: t -> t
(** [truncate a] returns the integer interval containing [a], that is
    [{low=floor a.low; high=ceil a.high}]. *)

val abs: t -> t
(** [abs a] returns the absolute value of the interval, that is
    - [{low=a.low; high=a.high}] if [a.low>=0.],
    - [{low=-a.high; high=-a.low}] if [a.high<=0.], and
    - [{low=0.; high=max -a.low a.high}] otherwise. *)

val union: t -> t -> t
(** [union a b] returns the union of the intervals [a] and [b], that is
    [{low=min a.low b.low; high=max a.high b.high}]. *)

val max: t -> t -> t
(** [max a b] returns "maximum" of the intervals [a] and [b], that is
    [{low=max a.low b.low; high=max a.high b.high}]. *)

val min: t -> t -> t
(** [min a b] returns the "minimum" of the intervals [a] and [b], that is
    [{low=min a.low b.low;high=min a.high b.high}]. *)

val ( + ) : t -> t -> t
(** [a + b] returns [{low=a.low +. b.low; high=a.high +. b.high}]
   properly rounded. *)

val ( +. ): t -> float -> t
(** [a +$. x] returns [{low=a.low+.x; high=a.high+.x}] properly rounded. *)

val ( +: ): float -> t -> t
(** [x +.$ a] returns [{low=a.low+.x; high=a.high+.x}] properly rounded. *)

val ( - ): t -> t -> t
(** [a - b] returns [{low=a.low-.b.high;high=a.high-.b.low}] properly
   rounded. *)

val ( -. ): t -> float -> t
(** [a -$. x] returns [{low=a.low-.x;high=a.high-.x}] properly rounded. *)

val ( -: ): float -> t -> t
(** [x -.$ a] returns [{low=x-.a.high; high=x-.a.low}] properly rounded. *)

val ( ~- ): t -> t
(** [~- a] is the unary negation, it returns [{low=-a.high; high=-a.low}]. *)

val ( * ): t -> t -> t
(** [a * b] multiplies [a] by [b] according to interval arithmetic
   and returns the proper result.  If [a=zero] or [b=zero] then
   {!zero} is returned. *)

val ( *. ): float -> t -> t
(** [x *$. a] multiplies [a] by [x] according to interval arithmetic
   and returns the proper result.  If [x=0.] then [zero_I] is returned. *)

val ( *: ): t -> float -> t
(** [a *. x] multiplies [a] by [x] according to interval arithmetic
   and returns the proper result.  If [x=0.] then {!zero} is returned. *)

val ( / ): t -> t -> t
(** [a / b] divides the first interval by the second according to
   interval arithmetic and returns the proper result.
   Raise [Failure "/$"] if [b=zero]. *)

val ( /. ): t -> float -> t
(** [a /$. x] divides [a] by [x] according to interval arithmetic and
   returns the proper result.  Raise [Failure "/$."] if [x=0.]. *)
(* FIXME: should we raise Division_by_zero (declared in this module). *)

val ( /: ): float -> t -> t
(** [x /.$ a] divides [x] by [a] according to interval arithmetic and
   returns the result.  Raise [Failure "/.$"] if [a=zero]. *)

val inv: t -> t
(** [inv a] returns [1. /.$ a].
    Raise [Failure "inv_I"] if [a=zero_I] *)
(* FIXME Division_by_zero *)

val mod_f: t -> float -> t
(** [mod_f a f] returns [a] mod [f] according to interval arithmetic
   et OCaml [mod_float] definition.  Raise [Failure "mod_f"] if [f=0.]. *)
(* FIXME: Division_by_zero *)

val sqrt: t -> t
(** [sqrt a] returns [{low=sqrt a;high=sqrt b}] if [a>=0.],
   [{low=0.;high=sqrt b}] if [a<0.<=b].
   Raise [Failure "sqrt_I"] if [b<0.]. *)
(* FIXME: Domain_error *)

val ( ** ): t -> int -> t
(** [pow_i a n] returns interval [a] raised to [n]th power according
   to interval arithmetic.  If [n=0] then {!one} is returned.  Raise
   [Failure "pow_f"] if [n<=0] and [a=zero].  Computed with exp-log in
   base2. *)

val ( **. ): t -> float -> t
(** [a **$. f] returns interval [a] raised to f power according to
   interval arithmetic.  If [f=0.] then {!one} is
   returned. Raise [Failure "**$."] if [f<=0. and a=zero_I] or if [f]
   is not an integer value and [a.high < 0.].  Computed with exp-log in
   base2. *)

val ( **: ): float -> t -> t
(** [x **.$ a] returns float [x] raised to interval [a] power
   according to interval arithmetic, considering the restiction of x
   power y to x >= 0.  Raise [Failure "**.$"] if [x < 0] and [a.high
   <= 0]. *)

val ( *** ): t -> t -> t
(** [a **$ b] returns interval [a] raised to [b] power according to
   interval arithmetic, considering the restriction of x power y to
   x >= 0.  Raise [Failure "**$"] if [a.high < 0] or [(a.high=0. and
   b.high<=0.)] *)


(** {2 Logarithmic and exponential functions} *)

val log: t -> t
(** [log a] returns, properly rounded,
    - [{low=log a.low; high=log a.high}] if [a.low>0.], and
    - [{low=neg_infinity; high=log a.high}] if [a.low<0<=a.high].

    Raise [Failure "log_I"] if [a.high<=0.]. *)

val exp: t -> t
(** [exp a] returns [{low=exp a.high; high=exp b.high}], properly rounded. *)


(** {2 Trigonometric functions} *)

val cos: t -> t
(** [cos a] returns the proper extension of cos to interval arithmetic.
    Returns \[-1,1\] if one of the bounds is greater or lower than +/-2**53. *)

val sin: t -> t
(** [sin a] returns the proper extension of sin to interval arithmetic.
    Returns \[-1,1\] if one of the bounds is greater or lower than +/-2**53. *)

val tan: t -> t
(** [tan a]  returns the proper extension of tan to interval arithmetic.
    Returns \[-∞,∞\] if one of the bounds is greater or lower than +/-2**53. *)

val acos: t -> t
(** [acos a] raise [Failure "acos_I"] if [a.low>1. or a.high<-1.],
   else returns [{low=if a.high<1. then acos a.high else 0; high=if
   a.low>-1. then acos a.low else pi}].  All values are in \[0,π\].*)
(* FIXME: Domain_error *)

val asin: t -> t
(** [asin a] raise [Failure "asin_I"] if [a.low>1. or a.high<-1.]
   else returns [{low=if a.low>-1. then asin a.low else -pi/2; high=if
   a.low<1. then asin a.high else pi/2}].  All values are in
   \[-π/2,π/2\]. *)

val atan: t -> t
(** [atan a] returns [{low=atan a.low; high=atan a.high}] proprly
   rounded. *)

val atan2mod: t -> t -> t
(** [atan2mod y x] returns the proper extension of interval arithmetic
   to [atan2] but with values in \[-π, 2π\] instead of \[-π, π\].
   This can happen when [y.low < 0] and [y.high > 0] and [x.high < 0]:
   then the returned interval is [{low=atan2 y.high x.high;
   high=(atan2 y.low x.high)+2 pi}].  This preserves the best
   inclusion function possible but is not compatible with the standard
   definition of [atan2]. *)

val atan2: t -> t -> t
(** Same function as above but when [y.low < 0] and [y.high > 0] and
   [x.high < 0] the returned interval is \[-π, π\].  This does not
   preserve the best inclusion function but is compatible with the
   [atan2] regular definition. *)

val cosh: t -> t
(** [cosh] is the proper extension of cosh to interval arithmetic. *)

val sinh: t -> t
(** sinh is the proper extension of sinh to interval arithmetic. *)

val tanh: t -> t
(** tanh is the proper extension of tanh to interval arithmetic. *)


(** {2 Usual arithmetic operators} *)

(** Module undoing the redeclaration of usual infix operators [+],
   [+.], etc. in case it is needed locally, while this module is
   open.

   Example: [Interval.(x + sin(of_int U.(n + 1)))]. *)
module U : sig
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
end


(** {2 Arrays of intervals} *)

(** Operations on arrays of intervals. *)
module Arr : sig
  val size_max: t array -> float
  (** Computes the size of the largest interval of the interval vector. *)

  val size_mean: t array -> float
  (** Computes the mean of the size of intervals of the interval vector. *)

  val to_string : ?fmt: (float -> 'b, 'a, 'b) format -> t array -> string
  (** [to_string a] returns a string representation of [a].
      @param fmt is the format used to print the two bounds of [i].
                 Default: ["%g"]. *)

  val pr : out_channel -> t array -> unit
  (** Print the interval array to the channel.  To be used with
     [Printf] format "%a". *)

  val pp : Format.formatter -> t array -> unit
  (** Print the interval array to the formatter.  To be used with
     [Format] format "%a". *)

  val fmt : (float -> 'b, 'a, 'b) format -> (t array -> 'c, 'd, 'e, 'c) format4
end


(** Open this module to have the same interface than the older
   versions of [Interval]. *)
module Deprecated : sig
  type interval = t = {
    low: float; (** low bound, possibly = -∞ *)
    high: float (** high bound, possibly = +∞ *)
  }

  (** Neutral element for addition *)
  val zero_I : interval

  (** Neutral element for multiplication *)
  val one_I : interval

  (** [pi] with bounds properly rounded *)
  val pi_I: interval

  (** [e] with bounds properly rounded *)
  val e_I: interval

  (** Prints an interval with the same format applied to both
     endpoints.  Formats follow the same specification than the one
     used for the regular printf function *)
  val printf_I : (float -> string, unit, string) format -> interval -> unit

  (** Prints an interval into an out_channel with the same format
     applied to both endpoints *)
  val fprintf_I :
    out_channel -> (float -> string, unit, string) format -> interval -> unit

  (** Returns a string holding the interval printed with the same
     format applied to both endpoints *)
  val sprintf_I: (float -> string, unit, string) format -> interval -> string

  (** Returns the interval containing the float conversion of an
     integer *)
  val float_i: int -> interval

  (**  [compare_I_f a x] returns [1] if [a.high<x],
       [0] if [a.low<=x<=a.high] and [-1] if  [x<a.low]  *)
  val compare_I_f: interval -> float -> int

  (** [size_I a] returns [a.high-a.low] *)
  val size_I: interval -> float

  (** [sgn a] returns [{low=float (compare a.low 0.);high=float
     (compare a.high 0.)}] *)
  val sgn_I: interval -> interval

  (** [truncate_I a] returns [{low=floor a.low;high=ceil a.high}] *)
  val truncate_I: interval -> interval

  (** [abs_I a] returns [{low=a.low;high=a.high}] if [a.low>=0.],
     [{low=-a.high;high=-a.low}] if [a.high<=0.], and
     [{low=0.;high=max -a.low a.high}] otherwise *)
  val abs_I: interval -> interval

  (** [union_I_I a b] returns [{low=min a.low b.low; high=max a.high b.high}] *)
  val union_I_I: interval -> interval -> interval

  (** [max_I_I a b] returns [{low=max a.low b.low; high=max a.high b.high}] *)
  val max_I_I: interval -> interval -> interval

  (** [min_I_I a b] returns [{low=min a.low b.low; high=min a.high b.high}] *)
  val min_I_I: interval -> interval -> interval

  (** [a +$ b] returns [{low=a.low+.b.low;high=a.high+.b.high}] *)
  val (+$): interval -> interval -> interval

  (** [a +$. x] returns [{low=a.low+.x;high=a.high+.x}] *)
  val (+$.): interval -> float -> interval

  (** [x +.$ a] returns [{low=a.low+.x;high=a.high+.x}] *)
  val (+.$): float -> interval -> interval

  (** [a -$ b] returns [{low=a.low-.b.high;high=a.high-.b.low}] *)
  val (-$): interval -> interval -> interval

  (** [a -$. x] returns [{low=a.low-.x;high=a.high-.x}] *)
  val (-$.): interval -> float -> interval

  (** [x -.$ a] returns [{low=x-.a.high;high=x-.a.low}] *)
  val (-.$): float -> interval -> interval

  (** [~-$ a] returns [{low=-a.high;high=-a.low}] *)
  val (~-$): interval -> interval

  (** [a *$. x] multiplies [a] by [x] according to interval arithmetic
     and returns the proper result.  If [x=0.] then [zero_I] is
     returned *)
  val ( *$.): interval -> float -> interval

  (** [x *$. a] multiplies [a] by [x] according to interval arithmetic
     and returns the proper result.  If [x=0.] then [zero_I] is
     returned.  *)
  val ( *.$): float -> interval -> interval

  (** [a *$ b] multiplies [a] by [b] according to interval arithmetic
     and returns the proper result.  If [a=zero_I] or [b=zero_I] then
     [zero_I] is returned*)
  val ( *$): interval -> interval -> interval

  (** [a /$. x] divides [a] by [x] according to interval arithmetic
     and returns the proper result.  Raise [Failure "/$."] if [x=0.] *)
  val (/$.): interval -> float -> interval

  (** [x /.$ a] divides [x] by [a] according to interval arithmetic
     and returns the result.  Raise [Failure "/.$"] if [a=zero_I] *)
  val (/.$): float -> interval -> interval

  (** [a /$ b] divides the first interval by the second according to
     interval arithmetic and returns the proper result.  Raise
     [Failure "/$"] if [b=zero_I] *)
  val (/$): interval -> interval -> interval

  (** [mod_I_f a f] returns [a] mod [f] according to interval
     arithmetic et OCaml mod_float definition.  Raise [Failure
     "mod_I_f"] if [f=0.] *)
  val mod_I_f: interval -> float -> interval

  (** [inv_I a] returns [1. /.$ a].
      Raise [Failure "inv_I"] if [a=zero_I] *)
  val inv_I: interval -> interval

  (** [sqrt_I a] returns [{low=sqrt a;high=sqrt b}] if [a>=0.],
      [{low=0.;high=sqrt b}] if [a<0.<=b].
      Raise [Failure "sqrt_I"] if [b<0.] *)
  val sqrt_I: interval -> interval

  (** [Pow_I_i a n] with [n] integer returns interval [a] raised to
     nth power according to interval arithmetic.  If [n=0] then
     [{low=1.;high=1.}] is returned. Raise [Failure "pow_I_f"] if
     [n<=0] and [a=zero_I].  Computed with exp-log in base2. *)
  val pow_I_i: interval -> int -> interval

  (** [a **$. f] returns interval [a] raised to f power according to
      interval arithmetic.  If [f=0.] then [{low=1.;high=1.}] is returned.
      Raise [Failure "**$."] if [f<=0. and a=zero_I]
      or if [f is not an integer value and a.high<0.].
      Computed with exp-log in base2. *)
  val ( **$.): interval -> float -> interval

  (** [a **$ b] returns interval [a] raised to [b] power according to
     interval arithmetic, considering the restriction of x power y to
     x >= 0.  Raise [Failure "**$"] if [a.high < 0] or [(a.high=0. and
     b.high<=0.)] *)
  val ( **$): interval -> interval -> interval

  (** [x **.$ a] returns float [x] raised to interval [a] power
     according to interval arithmetic, considering the restiction of x
     power y to x >= 0.
     Raise [Failure "**.$"] if [x < 0] and [a.high <= 0]*)
  val ( **.$): float -> interval -> interval

  (** [log_I a] returns [{low=log a.low; high=log a.high}] if [a.low>0.],
      [{low=neg_infinity; high=log a.high}] if [a.low<0<=a.high].
      Raise [Failure "log_I"] if [a.high<=0.] *)
  val log_I: interval -> interval

  (** [exp_I a] returns [{low=exp a.high;high=exp b.high}] *)
  val exp_I: interval -> interval

  (** [cos_I a]  returns the proper extension of cos to arithmetic interval
      Returns \[-1,1\] if one of the bounds is greater or lower than +/-2**53 *)
  val cos_I: interval -> interval

  (** [sin_I a]  returns the proper extension of sin to arithmetic interval
      Returns \[-1,1\] if one of the bounds is greater or lower than +/-2**53 *)
  val sin_I: interval -> interval

  (** [tan_I a]  returns the proper extension of tan to arithmetic interval
      Returns \[-Inf,Inf\] if one of the bounds is greater or lower
      than +/-2**53 *)
  val tan_I: interval -> interval

  (** [acos_I a] raise [Failure "acos_I"] if [a.low>1. or a.high<-1.],
      else returns [{low=if a.high<1. then acos a.high else 0;
      high=if a.low>-1. then acos a.low else pi}].
      All values are in \[0,pi\].*)
  val acos_I: interval -> interval

  (** [asin_I a] raise [Failure "asin_I"] if [a.low>1. or a.high<-1.]
      else returns [{low=if a.low>-1. then asin a.low else -pi/2;
      high=if a.low<1. then asin a.high else pi/2}].
      All values are in \[-pi/2,pi/2\]. *)
  val asin_I: interval -> interval

  (** [atan_I a]  returns [{low=atan a.low;high=atan a.high}] *)
  val atan_I: interval -> interval

  (** [atan2mod_I_I y x] returns the proper extension of interval
     arithmetic to atan2 but with values in \[-pi,2 pi\] instead of
     \[-pi,pi\]. This can happen when y.low<0 and y.high>0 and
     x.high<0: then the returned interval is [{low=atan2 y.high
     x.high;high=(atan2 y.low x.high)+2 pi}]. This preserves the best
     inclusion function possible but is not compatible with the
     standard definition of atan2 *)
  val atan2mod_I_I: interval -> interval -> interval

  (** Same function as above but when y.low<0 and y.high>0 and
     x.high<0 the returned interval is \[-pi,pi\].  This does not
     preserve the best inclusion function but is compatible with the
     atan2 regular definition *)
  val atan2_I_I: interval -> interval -> interval

  (** cosh_I is the proper extension of interval arithmetic to cosh *)
  val cosh_I: interval -> interval

  (** sinh_I is the proper extension of interval arithmetic to sinh *)
  val sinh_I: interval -> interval

  (** tanh_I is the proper extension of interval arithmetic to tanh *)
  val tanh_I: interval -> interval

  (** Computes the size of the largest interval of the interval vector *)
  val size_max_X: interval array -> float

  (** Computes the mean of the size of intervals of the interval vector *)
  val size_mean_X: interval array -> float

  (** Prints an interval vector with the same format applied to all
     endpoints. *)
  val printf_X : (float -> string, unit, string) format ->
                 interval array -> unit

  (** Prints an interval vector into an out_channel
      with the same format applied to all endpoints *)
  val fprintf_X : out_channel -> (float -> string, unit, string) format ->
                  interval array -> unit

  (** Returns a string holding the interval vector printed with the
     same format applied to all endpoints *)
  val sprintf_X: (float -> string, unit, string) format ->
                 interval array -> string

  (** Deprecated *)
  val print_X: interval array -> unit

  (** Deprecated *)
  val print_I: interval -> unit

  (** Deprecated *)
  val size_X: interval array -> float

  (** Deprecated *)
  val size2_X: interval array -> float

  (** Deprecated *)
  val (<$.): interval -> float -> int

  (** Deprecated *)
  val pow_I_f : interval -> float -> interval

  (** Deprecated *)
  val pow_I_I : interval -> interval -> interval

end [@@deprecated "Make an alias module I = Interval"]

type interval = t [@@deprecated "Use Interval.t instead"]
(** Deprecated version of {!Interval.t}. *)

(** {2:perf Performance}

Intel Atom 230 Linux 32 bits:
{ul
{-      ftan speed (10000000 calls):2.528158}
{-      fcos speed (10000000 calls):2.076129}
{-      fsin speed (10000000 calls):1.972123}
{-     tan_I speed (10000000 calls):4.416276}
{-     cos_I speed (10000000 calls):4.936308}
{-     sin_I speed (10000000 calls):5.396338}
{-      fadd speed (10000000 calls):0.980062}
{-      fsub speed (10000000 calls):0.980061}
{-      fmul speed (10000000 calls):0.980061}
{-      fdiv speed (10000000 calls):1.424089}
{-        +$ speed (10000000 calls):1.656103}
{-        -$ speed (10000000 calls):1.636103}
{-        *$ speed (10000000 calls):4.568285}
{-        /$ speed (10000000 calls):4.552285}
}

Intel 980X Linux 64 bits:
{ul
{-      ftan speed (10000000 calls):0.472029}
{-      fcos speed (10000000 calls):0.400025}
{-      fsin speed (10000000 calls):0.400025}
{-     tan_I speed (10000000 calls):0.752047}
{-     cos_I speed (10000000 calls):1.036065}
{-     sin_I speed (10000000 calls):1.104069}
{-      fadd speed (10000000 calls):0.124008}
{-      fsub speed (10000000 calls):0.120008}
{-      fmul speed (10000000 calls):0.128008}
{-      fdiv speed (10000000 calls):0.156010}
{-        +$ speed (10000000 calls):0.340021}
{-        -$ speed (10000000 calls):0.332021}
{-        *$ speed (10000000 calls):0.556035}
{-        /$ speed (10000000 calls):0.468029}
}


 *)
;;
