open Interval_base

let () =
  assert(RoundDown.pow_i 2. 3 = 8.);
  assert(RoundDown.pow_i 2. (-2) = 0.25);
  assert(RoundDown.pow_i 0. 0 = 1.);
  assert(RoundDown.pow_i infinity 0 = 1.);
  assert(RoundDown.pow_i neg_infinity 0 = 1.);
  assert(RoundDown.pow_i nan 0 = 1.)

let () =
  assert(RoundUp.pow_i 2. 3 = 8.);
  assert(RoundUp.pow_i 2. (-2) = 0.25);
  assert(RoundUp.pow_i 0. 0 = 1.);
  assert(RoundUp.pow_i infinity 0 = 1.);
  assert(RoundUp.pow_i neg_infinity 0 = 1.);
  assert(RoundUp.pow_i nan 0 = 1.)

let () =
  assert(Float.is_nan(RoundUp.sqrt nan));
  assert(Float.is_nan(sqrt nan));
  assert(Float.is_nan(RoundDown.sqrt nan));
  assert(RoundUp.sqrt infinity = infinity);
  assert(sqrt infinity = infinity);
  assert(RoundDown.sqrt infinity = infinity);
  assert(1. /. RoundUp.sqrt 0. = infinity);
  assert(1. /. sqrt 0. = infinity);
  assert(1. /. RoundDown.sqrt 0. = infinity);
  assert(1. /. RoundUp.sqrt (-0.) = neg_infinity);
  assert(1. /. sqrt (-0.) = neg_infinity);
  assert(1. /. RoundDown.sqrt (-0.) = neg_infinity);
  let x = 3.9036066558023176e-154 in
  assert(RoundUp.sqrt x = 1.9757547053726885e-77);
  assert(sqrt x = 1.975754705372688e-77); (* round to nearest *)
  assert(RoundDown.sqrt x = 1.975754705372688e-77);
  let x = 3.90360665580231694e-154 in
  assert(RoundUp.sqrt x = 1.975754705372688e-77);
  assert(sqrt x = 1.975754705372688e-77);
  assert(RoundDown.sqrt x = 1.97575470537268771e-77)

let () =
  assert(I.(v 2. 2. ** 3 = v 8. 8.));
  assert(I.(v 2. 2. ** (-2) = v 0.25 0.25));
  assert(I.(v 0. 0. ** 0 = one));
  assert(I.(v neg_infinity infinity ** 0 = one));
  assert(I.((v (-1.) 0.)**4 = v 0. 1.))

let () =
  assert(try I.(ignore(inter_exn (v 0. 1.) (v 2. 3.))); false
         with Domain_error _ -> true);
  assert(I.(inter (v 0. 1.) (v 2. 3.)) = None)

let () =
  (* √2 = 0x1.6A09E667F3BCC_908B2F... *)
  assert(RoundUp.sqrt 2. = 0x1.6A09E667F3BCD);
  assert(RoundDown.sqrt 2.  = 0x1.6A09E667F3BCC);
  assert(I.sqrt(I.v (-1.) 1.) = I.v 0. 1.)

let () =
  assert(RoundUp.hypot infinity infinity = infinity);
  assert(RoundDown.hypot infinity infinity = infinity);
  assert(RoundUp.hypot (-3.) 4. = 5.);
  assert(RoundDown.hypot 3. (-4.) = 5.);
  assert(RoundUp.hypot (-1.) (-1.) = RoundUp.sqrt 2.);
  assert(RoundDown.hypot 1. 1. = RoundDown.sqrt 2.)
