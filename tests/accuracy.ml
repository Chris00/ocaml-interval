open Printf
open Interval_intel.Fpu

let () =
  let pi = acos(-1.) in
  printf "cos(%.10f) ≈ [%.17f, %.17f] (singleton: %b)\n"
    pi (Low.cos pi) (High.cos pi) (Low.cos pi = High.cos pi)
