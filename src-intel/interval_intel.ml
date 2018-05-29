(*
    Copyright 2011 Jean-Marc Alliot / Jean-Baptiste Gotteland

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

module Fpu = Fpu
module Low = Fpu.Low
module High = Fpu.High

type t = Interval_base.t = {low: float; high: float}

exception Division_by_zero = Interval_base.Division_by_zero
exception Domain_error = Interval_base.Domain_error

module I = struct
  include Interval_base.I       (* Redefines =, <=,... *)
  include Generic

  let mod_f {low = a; high = b} y =
    (* assume that the result of fmod is exact *)
    let sy = Pervasives.compare y 0. in
    let y = if U.(sy = 0) then raise Division_by_zero else abs_float y in
    if U.(0. <= a) then
      if U.(High.(b -. a) < y) then (
        let ma = Fpu.fmod a y and mb = Fpu.fmod b y in
        if U.(ma <= mb) then {low = ma; high = mb} else {low = 0.; high = y})
      else {low = 0.; high = y}
    else if U.(b <= 0.) then
      if U.(High.(b -. a) < y) then (
        let ma = Fpu.fmod a y and mb = Fpu.fmod b y in
        if U.(ma <= mb) then {low = ma; high = mb} else {low = -.y; high = 0.})
      else {low = -.y; high = 0.}
    else
      { low = if U.(a <= -.y) then -.y else Fpu.fmod a y;
        high = if U.(y <= b) then y else Fpu.fmod b y }

  let max_63 = ldexp 1. 63

  external cos: t -> t = "fcos_I_caml"
  external sin: t -> t = "fsin_I_caml"

  let tan {low = a; high = b} =
    let open U in
    if -.max_63 <= a && b <= max_63 && High.(b -. a) < pi.high then (
      let ta = Low.tan a in
      let tb = High.tan b in
      if ta <= tb then {low = ta; high = tb}
      else {low = neg_infinity; high = infinity})
    else {low = neg_infinity; high = infinity}

  let acos {low = a; high = b} =
    let open U in
    if a <= 1. && -1. <= b then
      {low = if b < 1. then Low.acos b else 0.;
       high = if -1. < a then High.acos a else pi.high}
    else raise(Domain_error "acos")

  let asin {low = a; high = b} =
    let open U in
    if a <= 1. && -1. <= b then
      { low = if -1. < a then Low.asin a else -.half_pi.high;
        high = if b < 1. then High.asin b else half_pi.high }
    else raise(Domain_error "asin")

  let atan {low = a; high = b} =
    { low = Low.atan2 a 1.; high = High.atan2 b 1.}

  let atan2mod {low = ya; high = yb} {low = xa; high = xb} =
    let open U in
    let sya = compare ya 0. and syb = compare yb 0. in
    let sxa = compare xa 0. and sxb = compare xb 0. in
    if syb < 0 then
      if sxb <= 0 then {low = Low.atan2 yb xa; high = High.atan2 ya xb}
      else if 0 <= sxa then {low = Low.atan2 ya xa; high = High.atan2 yb xb}
      else {low = Low.atan2 yb xa; high = High.atan2 yb xb}
    else if 0 < sya then
      if sxb <= 0 then {low = Low.atan2 yb xb; high = High.atan2 ya xa}
      else if 0 <= sxa then {low = Low.atan2 ya xb; high = High.atan2 yb xa}
      else {low = Low.atan2 ya xb; high = High.atan2 ya xa}
    else if sya = syb (* = 0. *) then
      if sxa = 0 && sxb = 0 then raise(Domain_error "atan2mod")
      else if 0 <= sxa then zero
      else if sxb <= 0 then pi
      else {low = 0.; high = pi.high}
    else if sya = 0 then
      { low = if sxb <= 0 then Low.atan2 yb xb else 0.;
        high = if 0 <= sxa then High.atan2 yb xa else pi.high}
    else if syb = 0 then
      { low = if 0 <= sxa then Low.atan2 ya xa else -.pi.high;
        high = if sxb <= 0 then High.atan2 ya xb else 0. }
    else if sxb <= 0 then
      {low = Low.atan2 yb xb; high = High.(atan2 ya xb +. two_pi)}
    else if 0 <= sxa then {low = Low.atan2 ya xa; high = High.atan2 yb xa}
    else {low = -.pi.high; high = pi.high}

  let atan2 {low = ya; high = yb} {low = xa; high = xb} =
    let open U in
    let sya = compare ya 0. and syb = compare yb 0. in
    let sxa = compare xa 0. and sxb = compare xb 0. in
    if syb < 0 then
      if sxb <= 0 then {low = Low.atan2 yb xa; high = High.atan2 ya xb}
      else if 0 <= sxa then {low = Low.atan2 ya xa; high = High.atan2 yb xb}
      else {low = Low.atan2 yb xa; high = High.atan2 yb xb}
    else if 0 < sya then
      if sxb <= 0 then {low = Low.atan2 yb xb; high = High.atan2 ya xa}
      else if 0 <= sxa then {low = Low.atan2 ya xb; high = High.atan2 yb xa}
      else {low = Low.atan2 ya xb; high = High.atan2 ya xa}
    else if sya = syb then
      if sxb <= 0 then
        if sxa = 0 then raise(Domain_error "atan2")
        else {low = pi.low; high = pi.high}
      else if 0 <= sxa then {low = 0.; high = 0.}
      else {low = 0.; high = pi.high}
    else if sya = 0 then
      { low = if 0 < sxb then 0. else Low.atan2 yb xb;
        high = if sxa < 0 then pi.high else High.atan2 yb xa }
    else if syb = 0 then
      { low = if sxa < 0 then -.pi.high else Low.atan2 ya xa;
        high = if 0 < sxb then 0. else High.atan2 ya xb }
    else if 0 <= sxa then {low = Low.atan2 ya xa; high = High.atan2 yb xa}
    else {low = -.pi.high; high = pi.high}


  module Arr = struct

    let size_mean v =
      let add sum {low = a; high = b} = High.(sum +. (b -. a)) in
      U.(Array.fold_left add 0. v /. float (Array.length v))

    let size_max v =
      Array.fold_left (fun m {low = a; high = b} -> fmax m High.(b -. a)) 0. v

    let pr ch v =
      if U.(Array.length v = 0) then Printf.fprintf ch "[| |]"
      else (
        Printf.fprintf ch "[| [%g, %g]" (v.(0)).low (v.(0)).high;
        for i = 1 to U.(Array.length v - 1) do
          Printf.fprintf ch "; [%g, %g]" (v.(i)).low (v.(i)).high;
        done;
        Printf.fprintf ch " |]";
      )

    let pp ch v =
      if U.(Array.length v = 0) then Format.fprintf ch "[| |]"
      else (
        Format.fprintf ch "[| [%g, %g]" (v.(0)).low (v.(0)).high;
        for i = 1 to U.(Array.length v - 1) do
          Format.fprintf ch "; [%g, %g]" (v.(i)).low (v.(i)).high;
        done;
        Format.fprintf ch " |]";
      )

    let pr_buffer b fmt i =
      Printf.bprintf b "[%(%f%), %(%f%)]" fmt i.low fmt i.high

    let add_buffer b fmt v =
      Buffer.add_string b "[| ";
      pr_buffer b fmt v.(0);
      for i = 1 to Pervasives.( - ) (Array.length v) 1 do
        Buffer.add_string b "; ";
        pr_buffer b fmt v.(i);
      done;
      Buffer.add_string b " |]"

    let to_string_fmt fmt v =
      if U.(Array.length v = 0) then "[| |]"
      else (
        let b = Buffer.create 256 in
        add_buffer b fmt v;
        Buffer.contents b
      )

    let to_string ?(fmt=("%g": _ format)) v = to_string_fmt fmt v

    let fmt fmt_float =
      let open CamlinternalFormatBasics in
      let to_string () v = to_string_fmt fmt_float v in
      let fmt = Custom(Custom_succ Custom_zero, to_string, End_of_format) in
      Format(fmt , "Inverval.Arr.t")
  end
end

let zero_I = I.zero
let one_I = I.one
let pi_I = I.pi
let e_I = I.e

let sprintf_I format i =
  Printf.sprintf "[%s, %s]"
    (Printf.sprintf format i.low) (Printf.sprintf format i.high)

let fprintf_I fp format i =
  Printf.fprintf fp "[%s, %s]"
    (Printf.sprintf format i.low) (Printf.sprintf format i.high)

let printf_I format i =
  Printf.fprintf stdout "[%s, %s]"
    (Printf.sprintf format i.low) (Printf.sprintf format i.high)

let float_i = I.of_int
let compare_I_f = I.compare_f
let size_I x = x.high -. x.low
let sgn_I = I.sgn
let truncate_I = I.truncate
let abs_I = I.abs
let union_I_I = I.hull
let max_I_I = I.max
let min_I_I = I.min
let ( +$ ) = I.( + )
let ( +$. ) = I.( +. )
let ( +.$ ) = I.( +: )
let ( -$ ) = I.( - )
let ( -$. ) = I.( -. )
let ( -.$ ) = I.( -: )
let ( ~-$ ) = I.( ~- )
let ( *$. ) = I.( *: )
let ( *.$ ) = I.( *. )
let ( *$ ) = I.( * )
let ( /$. ) x y = try I.(x /. y) with Division_by_zero -> failwith "/$."
let ( /.$ ) x y = try I.(x /: y) with Division_by_zero -> failwith "/.$"
let ( /$ ) x y = try I.(x / y) with Division_by_zero -> failwith "/$"
let mod_I_f x y = try I.mod_f x y with Division_by_zero -> failwith "mod_I_f"
let inv_I x = try I.inv x with Division_by_zero -> failwith "inv_I"
let sqrt_I x = try I.sqrt x with Domain_error _ -> failwith "sqrt_I"
let pow_I_i x y = try I.(x ** y) with Domain_error _ -> failwith "pow_I_i"
let ( **$. ) x y = try I.(x **. y) with Domain_error _ -> failwith "**$."
let ( **.$ ) x y = try I.(x **: y) with Domain_error _ -> failwith "**.$"
let ( **$ ) x y = try I.(x *** y) with Domain_error _ -> failwith "**$"
let log_I x = try I.log x with Domain_error _ -> failwith "log_I"
let exp_I = I.exp
let cos_I = I.cos
let sin_I = I.sin
let tan_I = I.tan
let acos_I x = try I.acos x with Domain_error _ -> failwith "acos_I"
let asin_I x = try I.asin x with Domain_error _ -> failwith "asin_I"
let atan_I = I.atan
let atan2mod_I_I x y = try I.atan2mod x y
                       with Domain_error _ -> failwith "atan2mod_I_I"
let atan2_I_I x y = try I.atan2 x y with Domain_error _ -> failwith "atan2_I_I"
let cosh_I = I.cosh
let sinh_I = I.sinh
let tanh_I = I.tanh
let size_max_X = I.Arr.size_max
let size_mean_X = I.Arr.size_mean

let print_I x = Printf.printf "[%f, %f] " x.low x.high
let print_X v = Array.iter print_I v

let printf_X format v = Array.iter (printf_I format) v
let fprintf_X fp format v = Array.iter (fprintf_I fp format) v
let sprintf_X format v =
  Array.fold_left (fun  s x -> (sprintf_I format x) ^ s) "" v

let (<$.) = I.compare_f
let size_X = size_max_X
let size2_X = size_mean_X
let pow_I_I = ( **$ )
let pow_I_f = ( **$. )


type interval = t [@@deprecated]
