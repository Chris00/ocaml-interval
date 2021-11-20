open Printf
open Interval_base

let rnd_values = Array.init 1000 (fun _ -> 1000. -. Random.float 2000.)
let rnd_values2 = Array.init 1000 (fun _ -> 1000. -. Random.float 2000.)

let rnd_values_I =
  Array.init 1000 (fun _ ->
      let x1 = 1000. -. Random.float 2000. and x2 = Random.float 10. in
      I.v x1 (x1 +. x2))

let rnd_values2_I =
  Array.init 1000 (fun _ ->
      let x1 = 1000. -. Random.float 2000. and x2 = Random.float 10. in
      I.v x1 (x1 +. x2))

let rnd_values_pos_I =
  Array.init 1000 (fun _ ->
      let x1 = Random.float 2000. and x2 = Random.float 10. in
      I.v x1 (x1 +. x2))

let time_of1 n f =
  let top = (Unix.times ()).Unix.tms_utime in
  for _ = 1 to n do
    Array.iter (fun x -> ignore (f x)) rnd_values
  done;
  (Unix.times ()).Unix.tms_utime -. top

let time_of2 ?(incr=false) n f =
  let rnd2 = if incr then Array.map (fun x -> x +. 1.) rnd_values
             else rnd_values2 in
  let top = (Unix.times ()).Unix.tms_utime in
  for _ = 1 to n do
    Array.iter2 (fun x y -> ignore (f x y)) rnd_values rnd2;
  done;
  (Unix.times ()).Unix.tms_utime -. top

let time_of1_I ?(pos=false) n f =
  let rnd_I = if pos then rnd_values_pos_I else rnd_values_I in
  let top = (Unix.times ()).Unix.tms_utime in
  for _ = 1 to n do
    Array.iter (fun x -> ignore (f x)) rnd_I
  done;
  (Unix.times ()).Unix.tms_utime -. top

let time_of2_I n f =
  let top = (Unix.times ()).Unix.tms_utime in
  for _ = 1 to n do
    Array.iter2 (fun x y -> ignore (f x y)) rnd_values_I rnd_values2_I
  done;
  (Unix.times ()).Unix.tms_utime -. top

let speed_cmp1 ?pos n (name, f1, f2, f3, f4) =
  printf "%6s | %!" name;
  let t1 = time_of1 n f1 in
  printf "%8f | " t1;
  (match f2 with Some f2 -> let t2 = time_of1 n f2 in
                            printf "%8f %.1f×" t2 (t2 /. t1)
               | None -> printf "             ");
  let t3 = time_of1_I ?pos n f3 in
  printf " | %8f %.1f×" t3 (t3 /. t1);
  (match f4 with Some f4 -> let t4 = time_of1_I ?pos n f4 in
                            printf " | %8f %.1f×\n%!" t4 (t4 /. t1)
               | None -> printf "\n%!")


let speed_cmp2 n (name, f1, f2, f3) =
  printf "%6s | %!" name;
  let t1 = time_of2 n f1 in
  let t2 = time_of2 n f2 in
  printf "%8f | %8f %.1f× | " t1 t2 (t2 /. t1);
  (match f3 with Some f3 -> let t3 = time_of2_I n f3 in
                            printf "%8f %.1f×\n%!" t3 (t3 /. t1)
               | None -> printf "\n%!")

let () =
  let n = 10_000 in
  printf "# Calls: %d\n" (n * Array.length rnd_values_I);
  printf "       |   Float  |  RoundUp      |  *_base.I\n%!";
  let module Fpu = Interval_intel.Fpu in
  let module Cr = Interval_crlibm.I in
  let module It = Interval_intel.I in
  List.iter (speed_cmp2 n) [
      ("+", ( +. ),  RoundUp.( +. ), Some I.( + ));
      ("-", ( -. ),  RoundUp.( -. ), Some I.( - ));
      ("*", ( *. ),  RoundUp.( *. ), Some I.( * ));
      ("/", ( /. ),  RoundUp.( /. ), Some I.( / ));
      ("hypot", hypot, RoundUp.hypot, Some I.hypot);
    ];
  List.iter (speed_cmp1 n) [
      ("x^2", (fun x -> x**2.), Some(fun x -> RoundUp.pow_i x 2),
       I.(fun x -> x**2), None);
      ("x^3", (fun x -> x**3.), Some(fun x -> RoundUp.pow_i x 3),
       I.(fun x -> x**3), None);
      ("x^4", (fun x -> x**4.), Some(fun x -> RoundUp.pow_i x 4),
       I.(fun x -> x**4), None);
    ];
  let t_create1 = time_of2 n (fun x y -> {low = x;  high = y}) in
  let t_create2 = time_of2 n I.v ~incr:true in
  let t_create3 = time_of1 n I.singleton in
  printf "Create: {...}: %f  I.v: %f %.1f×  I.v: %f %.1f×\n\n%!"
    t_create1 t_create2 (t_create2 /. t_create1)
    t_create3 (t_create3 /. t_create1);
  printf "       |   Float  |  RoundUp      | *_crlibm.I    | *_intel.I\n%!";
  List.iter (speed_cmp1 n) [
      ("sin", sin, Some Crlibm.RoundUp.sin, Cr.sin, Some It.sin);
      ("sinpi", (fun x -> sin(Float.pi *. x)), Some Crlibm.RoundUp.sinpi,
                Cr.sinpi, None);
      ("cos", cos, Some Crlibm.RoundUp.cos, Cr.cos, Some It.cos);
      ("cospi", (fun x -> cos(Float.pi *. x)), Some Crlibm.RoundUp.cospi,
                Cr.cospi, None);
      ("tan", tan, Some Crlibm.RoundUp.tan, Cr.tan, Some It.tan);
      ("exp", exp, Some Crlibm.RoundUp.exp, Cr.exp, Some It.exp);
      ("expm1", expm1, Some Crlibm.RoundUp.expm1, Cr.expm1, None);
    ];
  List.iter (speed_cmp1 ~pos:true n) [
      ("log",   log,   Some Crlibm.RoundUp.log,   Cr.log,   Some It.log);
      ("log1p", log1p, Some Crlibm.RoundUp.log1p, Cr.log1p, None);
    ]



(* Local Variables: *)
(* compile-command: "dune build tests_speed.exe" *)
(* End: *)
