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

(* [min] and [max], specialized to floats (faster).
   NaN do dot need to be handled (see [I.v]). *)
let[@inline] fmin (a: float) (b: float) = if a <= b then a else b
let[@inline] fmax (a: float) (b: float) = if a <= b then b else a

type t = Interval.t = { low: float;  high: float }

module type DIRECTED = sig
  include Interval.DIRECTED with type t = float
  include Crlibm.S

  val tanh : t -> t

  module U = Interval.I.U
end

module Low = struct
  include Interval.Low  (* +, -,... *)
  include Crlibm.Low

  (* [Crlibm.tanh] does not exists.  The bound here may not be the
     tightest. *)
  let tanh x =
    if x >= 0. then
      let em1 = Crlibm.High.expm1(-2. *. x) in
      (-. em1) /. Interval.High.(2. +. em1)
    else
      let em1 = expm1(2. *. x) in
      em1 /. (em1 +. 2.)
end

module High = struct
  include Interval.High
  include Crlibm.High

  let tanh x =
    if x >= 0. then
      let em1 = Crlibm.Low.expm1(-2. *. x) in
      (-. em1) /. Interval.Low.(2. +. em1)
    else
      let em1 = expm1(2. *. x) in
      em1 /. (em1 +. 2.)
end

module I = struct
  include Interval.I  (* Redefines inequalities for intervals *)

  let mone_one = { low = -1.;  high = 1. }

  (* ASSUMING [x] is an integer value, [is_odd x] says whether it is odd. *)
  external is_odd : (float [@unboxed]) -> bool
    = "interval_is_odd_bc" "interval_is_odd" [@@noalloc]

  let cos { low = a; high = b } =
    let open U in
    let k = floor Low.(a /. High.pi) in
    let l = floor High.(b /. Low.pi) in
    if is_odd k then
      if l = k then
        (* It is guaranteed that kπ ≤ a ≤ b ≤ (k+1)π. *)
        {low = Low.cos a;  high = High.cos b} (* increasing *)
      else if l = k +. 1. then
        {low = fmin (Low.cos a) (Low.cos b);  high = 1.}
      else mone_one
    else (* k even *)
      if l = k then {low = Low.cos b;  high = High.cos a} (* decreasing *)
      else if l = k +. 1. then
        {low = -1.;  high = fmax (High.cos a) (High.cos b)}
      else mone_one

  let sin { low = a; high = b } =
    let open U in
    let k = floor Low.(a /. High.pi -. 0.5) in
    let l = floor High.(b /. Low.pi -. 0.5) in
    if is_odd k then
      if l = k then {low = Low.sin a;  high = High.sin b} (* increasing *)
      else if l = k +. 1. then
        {low = fmin (Low.sin a) (Low.sin b);  high = 1.}
      else mone_one
    else
      if l = k then {low = Low.sin b;  high = High.sin a } (* decreasing *)
      else if l = k +. 1. then
        {low = -1.;  high = fmax (High.sin a) (High.sin b)}
      else mone_one

  let atan {low = a; high = b} =
    { low = Low.atan a; high = High.atan b}

  let tanh {low = a; high = b} =
    { low = Low.tanh a; high = High.tanh b }

  include Generic (* Last because redefines [Low] and [High] as the
                     CRlibm ones (generated during build). *)
end
