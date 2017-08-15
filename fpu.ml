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

let _ = Callback.register_exception "failure" (Failure "")

external set_low: unit -> unit = "set_low" [@@noalloc]
external set_high: unit -> unit = "set_high" [@@noalloc]
external set_nearest: unit -> unit = "set_nearest" [@@noalloc]

external ffloat: int -> float = "ffloat_caml"
external ffloat_high: int -> float = "ffloat_high_caml"
external ffloat_low: int -> float = "ffloat_low_caml"

external fadd: float -> float -> float = "fadd_caml" "fadd" [@@unboxed]
external fadd_low: float -> float -> float
  = "fadd_low_caml" "fadd_low" [@@unboxed]
external fadd_high: float -> float -> float
  = "fadd_high_caml" "fadd_high" [@@unboxed]
external fsub: float -> float -> float = "fsub_caml" "fsub" [@@unboxed]
external fsub_low: float -> float -> float
  = "fsub_low_caml" "fsub_low" [@@unboxed]
external fsub_high: float -> float -> float
  = "fsub_high_caml" "fsub_high" [@@unboxed]
external fmul: float -> float -> float = "fmul_caml" "fmul" [@@unboxed]
external fmul_low: float -> float -> float
  = "fmul_low_caml" "fmul_low" [@@unboxed]
external fmul_high: float -> float -> float
  = "fmul_high_caml" "fmul_high" [@@unboxed]
external fdiv: float -> float -> float = "fdiv_caml" "fdiv" [@@unboxed]
external fdiv_low: float -> float -> float
  = "fdiv_low_caml" "fdiv_low" [@@unboxed]
external fdiv_high: float -> float -> float
  = "fdiv_high_caml" "fdiv_high" [@@unboxed]

external fmod: float -> float -> float = "fprem_caml" "fprem" [@@unboxed]

external fsqrt: float -> float = "fsqrt_caml" "fsqrt" [@@unboxed]
external fsqrt_low: float -> float = "fsqrt_low_caml" "fsqrt_low" [@@unboxed]
external fsqrt_high: float -> float = "fsqrt_high_caml" "fsqrt_high" [@@unboxed]

external flog: float -> float = "flog_caml" "flog" [@@unboxed]
external flog_low: float -> float = "flog_low_caml" "flog_low" [@@unboxed]
external flog_high: float -> float = "flog_high_caml" "flog_high" [@@unboxed]

external fexp: float -> float = "fexp_caml" "fexp" [@@unboxed]
external fexp_low: float -> float = "fexp_low_caml" "fexp_low" [@@unboxed]
external fexp_high: float -> float = "fexp_high_caml" "fexp_high" [@@unboxed]

external flog_pow: float -> float -> float
  = "flog_pow_caml" "flog_pow" [@@unboxed]
external flog_pow_low: float -> float -> float
  = "flog_pow_low_caml" "flog_pow_low" [@@unboxed]
external flog_pow_high: float -> float -> float
  = "flog_pow_high_caml" "flog_pow_high" [@@unboxed]

external fsin: float -> float = "fsin_caml" "fsin" [@@unboxed]
external fsin_low: float -> float = "fsin_low_caml" "fsin_low" [@@unboxed]
external fsin_high: float -> float = "fsin_high_caml" "fsin_high" [@@unboxed]
external fcos: float -> float = "fcos_caml" "fcos" [@@unboxed]
external fcos_low: float -> float = "fcos_low_caml" "fcos_low" [@@unboxed]
external fcos_high: float -> float = "fcos_high_caml" "fcos_high" [@@unboxed]
external ftan: float -> float = "ftan_caml" "ftan" [@@unboxed]
external ftan_low: float -> float = "ftan_low_caml" "ftan_low" [@@unboxed]
external ftan_high: float -> float = "ftan_high_caml" "ftan_high" [@@unboxed]

external fasin: float -> float = "fasin_caml" "fasin" [@@unboxed]
external fasin_low: float -> float = "fasin_low_caml" "fasin_low" [@@unboxed]
external fasin_high: float -> float = "fasin_high_caml" "fasin_high" [@@unboxed]
external facos: float -> float = "facos_caml" "facos" [@@unboxed]
external facos_low: float -> float = "facos_low_caml" "facos_low" [@@unboxed]
external facos_high: float -> float = "facos_high_caml" "facos_high" [@@unboxed]
external fatan: float -> float -> float = "fatan_caml" "fatan" [@@unboxed]
external fatan_low: float -> float -> float
  = "fatan_low_caml" "fatan_low" [@@unboxed]
external fatan_high: float -> float -> float
  = "fatan_high_caml" "fatan_high" [@@unboxed]

external fsinh: float -> float = "fsinh_caml" "fsinh" [@@unboxed]
external fsinh_low: float -> float = "fsinh_low_caml" "fsinh_low" [@@unboxed]
external fsinh_high: float -> float = "fsinh_high_caml" "fsinh_high" [@@unboxed]
external fcosh: float -> float = "fcosh_caml" "fcosh" [@@unboxed]
external fcosh_low: float -> float = "fcosh_low_caml" "fcosh_low" [@@unboxed]
external fcosh_high: float -> float = "fcosh_high_caml" "fcosh_high" [@@unboxed]
external ftanh: float -> float = "ftanh_caml" "ftanh" [@@unboxed]
external ftanh_low: float -> float = "ftanh_low_caml" "ftanh_low" [@@unboxed]
external ftanh_high: float -> float = "ftanh_high_caml" "ftanh_high" [@@unboxed]

external is_neg: float -> bool = "is_neg_caml"


let inf_pow y =
  if y < 0. then 0. else if y = 0. then 1. else infinity

let zero_pow x y =
  if 0. < y then 0.
  else if y = 0. || y = neg_infinity || (is_neg x && floor y <> y) then nan
  else if is_neg x && mod_float y 2. <> 0. then neg_infinity
  else infinity

let neg_inf_pow y =
  if classify_float y = FP_infinite || floor y <> y then nan
  else if y = 0. then 1. else if y < 0. then 0.
  else if mod_float y 2. = 0. then infinity else neg_infinity

let pos_pow_inf x =
  if x < 1. then 0. else if x = 1. then 1. else infinity

let pos_pow_neg_inf x =
  if x < 1. then infinity else if x = 1. then 1. else 0.

let fpow x y =
  if x = infinity then inf_pow y
  else if 0. < x then
    if y = infinity then pos_pow_inf x
    else if y = neg_infinity then pos_pow_neg_inf x
    else flog_pow x y
  else if x = 0. then zero_pow x y
  else if x = neg_infinity then neg_inf_pow y
  else if classify_float y = FP_infinite || floor y <> y then nan
  else if mod_float y 2. = 0. then flog_pow (-.x) y
  else -.flog_pow (-.x) y

let fpow_low x y =
  if x = infinity then inf_pow y
  else if 0. < x then
    if y = infinity then pos_pow_inf x
    else if y = neg_infinity then pos_pow_neg_inf x
    else flog_pow_low x y
  else if x = 0. then zero_pow x y
  else if x = neg_infinity then neg_inf_pow y
  else if classify_float y = FP_infinite || floor y <> y then nan
  else if mod_float y 2. = 0. then flog_pow_low (-.x) y
  else -.flog_pow_high (-.x) y

let fpow_high x y =
  if x = infinity then inf_pow y
  else if 0. < x then
    if y = infinity then pos_pow_inf x
    else if y = neg_infinity then pos_pow_neg_inf x
    else flog_pow_high x y
  else if x = 0. then zero_pow x y
  else if x = neg_infinity then neg_inf_pow y
  else if classify_float y = FP_infinite || floor y <> y then nan
  else if mod_float y 2. = 0. then flog_pow_high (-.x) y
  else -.flog_pow_low (-.x) y
