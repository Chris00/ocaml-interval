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

module Low = struct
  include Interval.Low  (* +, -,... *)
  include Crlibm.Low
end

module High = struct
  include Interval.High
  include Crlibm.High
end

module I = struct
  include Interval.I  (* Redefines inequalities for intervals *)

  let mone_one = v (-1.) 1.

  (* ASSUMING [x] is an integer value, [is_even x] says whether it is even. *)
  external is_odd : (float [@unboxed]) -> bool
    = "interval_is_odd_bc" "interval_is_odd" [@@noalloc]

  let cos { low = a; high = b } =
    let open U in
    let k = floor Low.(a /. High.pi) in
    let l = floor High.(b /. Low.pi) in
    if is_odd k then
      if l = k then {low = Low.cos a;  high = High.cos b} (* increasing *)
      else if l = l +. 1. then
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
    (* [Crlibm.tanh] does not exists.  The bounds here may not be the
       tightest. *)
    let low = if U.(a >= 0.) then Low.(sinh a /. High.cosh a)
              else Low.(sinh a /. cosh a) in
    let high = if U.(b >= 0.) then High.(sinh b /. Low.cosh b)
               else High.(sinh b /. cosh b)in
    { low; high }

  include Generic (* Last because redefines [Low] and [High] as the
                     CRlibm ones (generated during build). *)
end
