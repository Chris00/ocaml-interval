open Interval_intel

let () =
  assert I.(exp (v neg_infinity 0.) = v 0. 1.);
  assert I.(exp (v 0. infinity) = v 1. infinity)
