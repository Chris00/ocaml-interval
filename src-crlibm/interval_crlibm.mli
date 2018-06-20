(*
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

(** Interval library for OCaml (crlibm version).

   This library is slightly slower than Interval but has the important
   property that the functions are proved correct (while Interval
   mostly uses the processor implementation, some of which are
   flawed).  Some additional functions are also available, thanks to
   CRlibm.  *)

type t = Interval.t = {
    low: float; (** low bound, possibly = -∞ *)
    high: float (** high bound, possibly = +∞ *)
  }

(** Interval operations.  Locally open this module — using
   e.g. [I.(...)] — to redefine classical arithmetic operators for
   interval arithmetic. *)
module I : sig
  include module type of Interval.I

  (* val mod_f: t -> float -> t *)

  (** {2 Logarithmic and exponential functions} *)

  val log: t -> t
  (** [log a] returns, properly rounded,
      - [{low=log a.low; high=log a.high}] if [a.low>0.], and
      - [{low=neg_infinity; high=log a.high}] if [a.low<0<=a.high].

      Raise [Domain_error] if [a.high] ≤ 0. *)

  val exp: t -> t
  (** [exp a] returns [{low=exp a.high; high=exp b.high}], properly rounded. *)


  val tan: t -> t
  (** [tan a]  returns the proper extension of tan to interval arithmetic.
      Returns \[-∞,∞\] if one of the bounds is greater or lower than ±2⁵³. *)

  val acos: t -> t
  (** [acos a] returns [{low=(if a.high<1. then acos a.high else 0);
     high=(if a.low>-1. then acos a.low else pi)}].
     All values are in \[0,π\].

     @raise Domain_error if [a.low > 1.] or [a.high < -1.] *)

  val asin: t -> t
  (** [asin a] returns [{low=(if a.low > -1. then asin a.low else -pi/2);
     high=(if a.low < 1. then asin a.high else pi/2)}].
     All values are in \[-π/2,π/2\].

     @raise Domain_error if [a.low > 1.] or [a.high < -1.] *)

  val atan: t -> t
  (** [atan a] returns [{low=atan a.low; high=atan a.high}] properly
     rounded. *)


  (** {2 Hyperbolic functions} *)

  val cosh: t -> t
  (** [cosh] is the proper extension of cosh to interval arithmetic. *)

  val sinh: t -> t
  (** sinh is the proper extension of sinh to interval arithmetic. *)

  val tanh: t -> t
  (** tanh is the proper extension of tanh to interval arithmetic. *)
end

module Low : sig
  include module type of Interval.Low
  include module type of Crlibm.Low
end

module High : sig
  include module type of Interval.High
  include module type of Crlibm.High
end
