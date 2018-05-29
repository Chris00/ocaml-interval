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

(** Implementation of interval functions that can be shared with the
   Interval_crlibm library.  (Trigonometric functions depend on
   argument reduction which is performed differently.) *)

open Interval_base
module Low = Fpu.Low
module High = Fpu.High

let[@inline] mod2 x = Fpu.fmod x 2.

(* [min] and [max], specialized to floats (faster).
   NaN do dot need to be handled. *)
let fmin (a: float) (b: float) = if a <= b then a else b
let fmax (a: float) (b: float) = if a <= b then b else a

let sqrt {low = a; high = b} =
  if b < 0. then raise(Domain_error "sqrt")
  else {low = if a < 0. then 0. else Low.sqrt a; high = High.sqrt b}

let ( ** ) {low = a; high = b} n =
  let nf = I.of_int n in
  let[@inline] pow_l x =
    if x = infinity then 0.
    else Low.pow x (if x < 1.0 then nf.high else nf.low) in
  let[@inline] pow_h x =
    if x = infinity then infinity
    else High.pow x (if x < 1.0 then nf.low else nf.high) in
  let sn = compare n 0 and sa = compare a 0. and sb = compare b 0. in
  if sn = 0 then if a = 0. && b = 0. then raise(Domain_error "**") else I.one
  else if sb < 0 then
    if n mod 2 = 0 then
      if 0 < sn then {low = pow_l (-.b); high = pow_h (-.a)}
      else {low = pow_l (-.a); high = pow_h (-.b)}
    else if 0 < sn then {low = -.pow_h (-.a); high = -.pow_l (-.b)}
    else {low = -.pow_h (-.b); high = -.pow_l (-.a)}
  else if 0 < sa then
    if 0 < sn then {low = pow_l a; high = pow_h b}
    else {low = pow_l b; high = pow_h a}
  else if n mod 2 = 0 then
    if 0 < sn then
      if sa = sb (* = 0. *) then {low = 0.; high = 0.}
      else {low = 0.; high = pow_h (fmax (-.a) b)}
    else if sa = sb (* = 0. *) then raise(Domain_error "**")
    else {low = pow_l (fmax (-.a) b); high = infinity}
  else if 0 < sn then
    { low = if sa = 0 then 0. else -.pow_h (-.a);
      high = if sb = 0 then 0. else pow_h b}
  else if sa = 0 then
    if sb = 0 then raise(Domain_error "**") else {low = pow_l b; high = infinity}
  else if sb = 0 then {low = neg_infinity; high = -.pow_l (-.a)}
  else {low = neg_infinity; high = infinity}

let ( **. ) {low = a; high = b} nf =
  let[@inline] pow_l x = if x = infinity then 0. else Low.pow x nf in
  let[@inline] pow_h x = if x = infinity then infinity else High.pow x nf in
  let sn = compare nf 0. and sa = compare a 0. and sb = compare b 0. in
  if sn = 0 then if a = 0. && b = 0. then raise(Domain_error "**.")
                 else I.one
  else if sb < 0 then
    if floor nf <> nf then raise(Domain_error "**.")
    else if mod2 nf = 0. then
      if 0 < sn then {low = Low.pow (-.b) nf; high = pow_h (-.a)}
      else {low = pow_l (-.a); high = High.pow (-.b) nf}
    else if 0 < sn then {low = -.pow_h (-.a); high = -. Low.pow (-.b) nf}
    else {low = -. High.pow (-.b) nf; high = -.pow_l (-.a)}
  else if 0 < sa then
    if 0 < sn then {low = Low.pow a nf; high = pow_h b}
    else {low = pow_l b; high = High.pow a nf}
  else if floor nf <> nf then
    if 0 < sn then {low = 0.; high = if sb = 0 then 0. else pow_h b}
    else if sb = 0 then raise(Domain_error "**.")
    else {low = pow_l b; high = infinity}
  else if mod2 nf = 0. then
    if 0 < sn then
      if sa = sb (* = 0. *) then {low = 0.; high = 0.}
      else {low = 0.; high = pow_h (fmax (-.a) b)}
    else if sa = sb (* = 0. *) then raise(Domain_error "**.")
    else {low = pow_l (fmax (-.a) b); high = infinity}
  else if 0 < sn then
    { low = if sa = 0 then 0. else -.pow_h (-.a);
      high = if sb = 0 then 0. else pow_h b}
  else if sa = 0 then
    if sb = 0 then raise(Domain_error "**.") else {low = pow_l b; high = infinity}
  else if sb = 0 then {low = neg_infinity; high = -.pow_l (-.a)}
  else {low = neg_infinity; high = infinity}

let ( *** ) {low = a; high = b} {low = c; high = d} =
  let a = fmax 0. a in
  if b < 0. then raise(Domain_error "***")
  else if b = 0. then
    if d <= 0. then raise(Domain_error "***") else {low = 0.; high = 0.}
  else if a = 0. then
    if 0. <= c then
        {low = if d = 0. then 1. else 0.;
         high = High.(b**(if b < 1. then c else d))}
    else if d <= 0. then
      {low = Low.(b**(if b < 1. then d else c)); high = infinity}
    else {low = 0.; high = infinity}
  else if 0. <= c then
    { low = Low.(a**(if a < 1. then d else c));
      high = High.(b**(if b < 1. then c else d)) }
  else if d <= 0. then
    { low = Low.(b**(if b < 1. then d else c));
      high = High.(a**(if a < 1. then c else d)) }
  else if b < 1. then {low = Low.(a**d); high = High.(a**c)}
    else if 1. < a then {low = Low.(b**c); high = High.(b**d)}
  else { low = fmin Low.(a**d) Low.(b**c);
         high = fmax High.(a**c) High.(b**d)}

let ( **: ) x {low = a; high = b} =
  if x = 0. && 0. < b then {low = 0.; high = 0.}
  else if x <= 0. then raise(Domain_error "**:")
  else if x < 1. then
    if a = neg_infinity then
      if b = infinity then {low = 0.; high = infinity}
      else {low = Low.pow x b; high = infinity}
    else if b = infinity then {low = 0.; high = High.pow x a}
    else {low = Low.pow x b; high = High.pow x a}
  else if x = 1. then {low = 1.; high = 1.}
  else if a = neg_infinity then
    if b = infinity then {low = 0.; high = infinity}
    else {low = 0.; high = High.pow x b}
  else if b = infinity then {low = Low.pow x a; high = infinity}
  else {low = Low.pow x a; high = High.pow x b}

let log {low = a; high = b} =
  let sb = compare b 0. in
  if sb <= 0 then raise(Domain_error "log")
  else {low = if a <= 0. then neg_infinity else Low.log a; high = High.log b}

let exp {low = a; high = b} =
  { low = if a = neg_infinity then 0. else Low.exp a;
    high = if b = infinity then infinity else High.exp b}

let cosh {low = a; high = b} =
  if b < 0. then {low = Low.cosh b; high = High.cosh a}
  else if a < 0. then {low = 1.; high = High.cosh (fmax (-.a) b)}
  else {low = Low.cosh a; high = High.cosh b}

let sinh {low = a; high = b} = {low = Low.sinh a; high = High.sinh b}

let tanh {low = a; high = b} = {low = Low.tanh a; high = High.tanh b}
