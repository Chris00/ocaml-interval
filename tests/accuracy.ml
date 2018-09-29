open Format

let () =
  let open Interval_intel.Fpu in
  let pi = acos(-1.) in
  printf "Fpu:\ncos(%.10f) ≈ [%.17f, %.17f] (singleton: %b)\n"
    pi (Low.cos pi) (High.cos pi) (Low.cos pi = High.cos pi)

let () =
  let open Interval_crlibm in
  let pi = acos(-1.) in
  printf "CRLibm:\ncos(%.10f) ∈ [%.17f, %.17f] (singleton: %b)\n"
    pi (Low.cos pi) (High.cos pi) (Low.cos pi = High.cos pi);
  let c = I.(cos pi) in
  printf ("cos([π]) = " ^^ I.fmt "%.17f" ^^ " (singleton: %b)\n")
    c (c.low = c.high)
