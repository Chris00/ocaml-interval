open Interval

let () =
  assert(Low.pow_i 2. 3 = 8.);
  assert(Low.pow_i 2. (-2) = 0.25);
  assert(Low.pow_i 0. 0 = 1.);
  assert(Low.pow_i infinity 0 = 1.);
  assert(Low.pow_i neg_infinity 0 = 1.);
  assert(Low.pow_i nan 0 = 1.)

let () =
  assert(High.pow_i 2. 3 = 8.);
  assert(High.pow_i 2. (-2) = 0.25);
  assert(High.pow_i 0. 0 = 1.);
  assert(High.pow_i infinity 0 = 1.);
  assert(High.pow_i neg_infinity 0 = 1.);
  assert(High.pow_i nan 0 = 1.)

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
  assert(High.sqrt 2. = 0x1.6A09E667F3BCD);
  assert(Low.sqrt 2.  = 0x1.6A09E667F3BCC);
  assert(I.sqrt(I.v (-1.) 1.) = I.v 0. 1.)

