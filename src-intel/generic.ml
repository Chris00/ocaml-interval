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

(** Implementation of interval functions that can be shared (see
   config/dispatch.ml) with the Interval_crlibm library.
   (Trigonometric functions depend on argument reduction which is
   performed differently.) *)

open Interval
module RoundDown = Fpu.RoundDown
module RoundUp = Fpu.RoundUp

let[@inline] mod2 x = Fpu.fmod x 2.

(* [min] and [max], specialized to floats (faster).
   NaN do dot need to be handled. *)
let fmin (a: float) (b: float) = if a <= b then a else b
let fmax (a: float) (b: float) = if a <= b then b else a

let log {low = a; high = b} =
  if b <= 0. then raise(Domain_error "log")
  else {low = if a <= 0. then neg_infinity else RoundDown.log a;
        high = RoundUp.log b}

let exp {low = a; high = b} =
  { low = RoundDown.exp a; high = RoundUp.exp b}

let max_63 = ldexp 1. 63

let tan {low = a; high = b} =
  if -.max_63 <= a && b <= max_63 && Interval.RoundUp.(b -. a < pi) then (
    let ta = RoundDown.tan a in
    let tb = RoundUp.tan b in
    if ta <= tb then {low = ta; high = tb}
    else Interval.I.entire)
  else Interval.I.entire

let acos {low = a; high = b} =
  if a <= 1. && -1. <= b then
    {low = if b < 1. then RoundDown.acos b else 0.;
     high = if -1. < a then RoundUp.acos a else Interval.RoundUp.pi}
  else raise(Domain_error "acos")

let asin {low = a; high = b} =
  if a <= 1. && -1. <= b then
    { low = if -1. < a then RoundDown.asin a else -. Interval.RoundUp.half_pi;
      high = if b < 1. then RoundUp.asin b else Interval.RoundUp.half_pi }
  else raise(Domain_error "asin")


let cosh {low = a; high = b} =
  if b < 0. then {low = RoundDown.cosh b; high = RoundUp.cosh a}
  else if a < 0. then {low = 1.; high = RoundUp.cosh (fmax (-.a) b)}
  else {low = RoundDown.cosh a; high = RoundUp.cosh b}

let sinh {low = a; high = b} = {low = RoundDown.sinh a; high = RoundUp.sinh b}
