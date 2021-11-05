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

type t = Interval_base.interval = { low: float;  high: float }

module type DIRECTED = sig
  include Interval_base.DIRECTED with type t = float
  include Crlibm.S

  val tanh : t -> t

  module U = Interval_base.I.U
end

module RoundDown = struct
  include Interval_base.RoundDown  (* +, -,... *)
  include Crlibm.RoundDown

  (* [Crlibm.tanh] does not exists.  The bound here may not be the
     tightest. *)
  let tanh x =
    if x >= 0. then
      let em1 = Crlibm.RoundUp.expm1(-2. *. x) in
      (-. em1) /. Interval_base.RoundUp.(2. +. em1)
    else
      let em1 = expm1(2. *. x) in
      em1 /. (em1 +. 2.)
end

module RoundUp = struct
  include Interval_base.RoundUp
  include Crlibm.RoundUp

  let tanh x =
    if x >= 0. then
      let em1 = Crlibm.RoundDown.expm1(-2. *. x) in
      (-. em1) /. Interval_base.RoundDown.(2. +. em1)
    else
      let em1 = expm1(2. *. x) in
      em1 /. (em1 +. 2.)
end

module I = struct
  include Interval_base.I  (* Redefines inequalities for intervals *)

  let mone_one = { low = -1.;  high = 1. }

  (* ASSUMING [x] is an integer value, [is_odd x] says whether it is odd. *)
  external is_odd : (float [@unboxed]) -> bool
    = "interval_is_odd_bc" "interval_is_odd" [@@noalloc]

  let cos { low = a; high = b } =
    let open U in
    let k = floor RoundDown.(a /. RoundUp.pi) in
    let l = floor RoundUp.(b /. RoundDown.pi) in
    if is_odd k then
      if l = k then
        (* It is guaranteed that kπ ≤ a ≤ b ≤ (k+1)π. *)
        {low = RoundDown.cos a;  high = RoundUp.cos b} (* increasing *)
      else if l = k +. 1. then
        {low = fmin (RoundDown.cos a) (RoundDown.cos b);  high = 1.}
      else mone_one
    else (* k even *)
      if l = k then {low = RoundDown.cos b;
                     high = RoundUp.cos a} (* decreasing *)
      else if l = k +. 1. then
        {low = -1.;  high = fmax (RoundUp.cos a) (RoundUp.cos b)}
      else mone_one

  let cospi { low = a; high = b } =
    let open U in
    let k = floor a in
    let l = floor b in
    if is_odd k then
      if l = k then
        (* It is guaranteed that k ≤ a ≤ b ≤ k+1. *)
        {low = RoundDown.cospi a;  high = RoundUp.cospi b} (* increasing *)
      else if l = k +. 1. then
        {low = fmin (RoundDown.cospi a) (RoundDown.cospi b);  high = 1.}
      else mone_one
    else (* k even *)
      if l = k then {low = RoundDown.cospi b;
                     high = RoundUp.cospi a} (* decreasing *)
      else if l = k +. 1. then
        {low = -1.;  high = fmax (RoundUp.cospi a) (RoundUp.cospi b)}
      else mone_one

  let sin { low = a; high = b } =
    let open U in
    let k = floor RoundDown.(a /. RoundUp.pi -. 0.5) in
    let l = floor RoundUp.(b /. RoundDown.pi -. 0.5) in
    if is_odd k then
      if l = k then {low = RoundDown.sin a;
                     high = RoundUp.sin b} (* increasing *)
      else if l = k +. 1. then
        {low = fmin (RoundDown.sin a) (RoundDown.sin b);  high = 1.}
      else mone_one
    else
      if l = k then {low = RoundDown.sin b;
                     high = RoundUp.sin a } (* decreasing *)
      else if l = k +. 1. then
        {low = -1.;  high = fmax (RoundUp.sin a) (RoundUp.sin b)}
      else mone_one

  let sinpi { low = a; high = b } =
    let open U in
    let k = floor RoundDown.(a -. 0.5) in
    let l = floor RoundUp.(b -. 0.5) in
    if is_odd k then
      if l = k then {low = RoundDown.sinpi a;
                     high = RoundUp.sinpi b} (* increasing *)
      else if l = k +. 1. then
        {low = fmin (RoundDown.sinpi a) (RoundDown.sinpi b);  high = 1.}
      else mone_one
    else
      if l = k then {low = RoundDown.sinpi b;
                     high = RoundUp.sinpi a } (* decreasing *)
      else if l = k +. 1. then
        {low = -1.;  high = fmax (RoundUp.sinpi a) (RoundUp.sinpi b)}
      else mone_one

  let max_63 = ldexp 1. 63

  let tanpi {low = a; high = b} =
    if U.(-.max_63 <= a && b <= max_63 && RoundUp.(b -. a < 1.)) then (
      let ta = RoundDown.tanpi a in
      let tb = RoundUp.tanpi b in
      if U.(ta <= tb) then {low = ta; high = tb}
      else entire)
    else entire

  let acospi {low = a; high = b} =
    if U.(a <= 1. && -1. <= b) then
      {low = if U.(b < 1.) then RoundDown.acospi b else 0.;
       high = if U.(-1. < a) then RoundUp.acospi a else 1.}
    else raise(Interval_base.Domain_error "acospi")

  let asinpi {low = a; high = b} =
    if U.(a <= 1. && -1. <= b) then
      { low = if U.(-1. < a) then RoundDown.asinpi a else -0.5;
        high = if U.(b < 1.) then RoundUp.asinpi b else 0.5 }
    else raise(Interval_base.Domain_error "asinpi")

  let atan {low = a; high = b} =
    { low = RoundDown.atan a; high = RoundUp.atan b}

  let atanpi {low = a; high = b} =
    { low = RoundDown.atanpi a; high = RoundUp.atanpi b}

  let tanh {low = a; high = b} =
    { low = RoundDown.tanh a; high = RoundUp.tanh b }

  let log1p {low = a; high = b} =
    if U.(b <= -1.) then raise(Interval_base.Domain_error "log1p")
    else {low = if U.(a <= -1.) then neg_infinity else RoundDown.log1p a;
          high = RoundUp.log1p b}

  let log2 {low = a; high = b} =
    if U.(b <= 0.) then raise(Interval_base.Domain_error "log2")
    else {low = if U.(a <= 0.) then neg_infinity else RoundDown.log2 a;
          high = RoundUp.log2 b}

  let log10 {low = a; high = b} =
    if U.(b <= 0.) then raise(Interval_base.Domain_error "log10")
    else {low = if U.(a <= 0.) then neg_infinity else RoundDown.log10 a;
          high = RoundUp.log10 b}

  let expm1 {low = a; high = b} =
    { low = RoundDown.expm1 a; high = RoundUp.expm1 b}

  include Generic (* Last because redefines [Low] and [High] as the
                     CRlibm ones (generated during build). *)
end


module Low = RoundDown
module High = RoundUp
