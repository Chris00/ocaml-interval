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
  include Interval.I



  let atan {low = a; high = b} =
    { low = Low.atan a; high = High.atan b}

  let tanh {low = a; high = b} =
    (* [Crlibm.tanh] does not exists.  The bounds here may not be the
       tightest. *)
    { low = Low.(sinh a /. High.cosh a);
      high = High.(sinh b /. Low.cosh b) }

  include Generic (* Last because redefines [Low] and [High] as the
                     CRlibm ones (generated during build). *)
end
