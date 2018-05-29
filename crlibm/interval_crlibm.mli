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

type t = Interval_base.t = {
    low: float; (** low bound, possibly = -∞ *)
    high: float (** high bound, possibly = +∞ *)
  }

(** Interval operations.  Locally open this module — using
   e.g. [I.(...)] — to redefine classical arithmetic operators for
   interval arithmetic. *)
module I : sig
  include module type of Interval_base.I

end

module Low = Crlibm.Low

module High = Crlibm.High
