open Printf
open Interval_base

let rnd_values = Array.init 1000 (fun _ -> 1000. -. Random.float 2000.)

let rnd_values_I =
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

let time_of2 n f =
  let top = (Unix.times ()).Unix.tms_utime in
  for _ = 1 to n do
    Array.iter (fun x -> ignore (f x x)) rnd_values;
  done;
  (Unix.times ()).Unix.tms_utime -. top

let time_of1_I ?(pos=false) n f =
  let rnd_I = if pos then rnd_values_pos_I else rnd_values_I in
  let top = (Unix.times ()).Unix.tms_utime in
  for _ = 1 to n do
    Array.iter (fun x -> ignore (f x)) rnd_I
  done;
  (Unix.times ()).Unix.tms_utime -. top

let time_of2_I ?(pos=false) n f =
  let rnd_I = if pos then rnd_values_pos_I else rnd_values_I in
  let top = (Unix.times ()).Unix.tms_utime in
  for _ = 1 to n do
    Array.iter (fun x -> ignore (f x x)) rnd_I
  done;
  (Unix.times ()).Unix.tms_utime -. top

let speed_cmp1 ?pos n (name, f1, f2, f3, f4) =
  printf "%6s | %!" name;
  let t1 = time_of1 n f1 in
  printf "%8f | " t1;
  (match f2 with Some f2 -> let t2 = time_of1 n f2 in
                            printf "%8f %.1f×" t2 (t2 /. t1)
               | None -> printf "            ");
  let t3 = time_of1_I ?pos n f3 in
  printf " | %8f %.1f× | " t3 (t3 /. t1);
  (match f4 with Some f4 -> let t4 = time_of1_I ?pos n f4 in
                            printf "%8f %.1f×\n" t4 (t4 /. t1)
               | None -> printf "\n%!")


let speed_cmp2 n (name, f1, f2, f3, f4) =
  printf "%6s | %!" name;
  let t1 = time_of2 n f1 in
  let t2 = time_of2 n f2 in
  printf "%8f | %8f %.1f× | " t1 t2 (t2 /. t1);
  (match f3 with Some f3 -> let t3 = time_of2_I n f3 in
                            printf "%8f %.1f× | " t3 (t3 /. t1)
               | None -> printf "              | ");
  (match f4 with Some f4 -> let t4 = time_of2_I n f4 in
                            printf "%8f %.1f×\n" t4 (t4 /. t1)
               | None -> printf "\n%!")

let () =
  let n = 10_000 in
  printf "# Calls: %d\n" (n * Array.length rnd_values_I);
  printf "       |   Float  |    Fpu        |  Crlibm       |   Intel\n%!";
  let module Fpu = Interval_intel.Fpu in
  let module Cr = Interval_crlibm.I in
  let module It = Interval_intel.I in
  List.iter (speed_cmp2 n) [
      ("+", ( +. ),  Fpu.fadd, Some Cr.( + ),  Some It.( + ));
      ("-", ( -. ),  Fpu.fsub, Some Cr.( - ),  Some It.( - ));
      ("*", ( *. ),  Fpu.fmul, Some Cr.( * ),  Some It.( * ));
      ("/", ( /. ),  Fpu.fdiv, Some Cr.( / ),  Some It.( / ));
      ("**", ( ** ), Fpu.fpow, None, None);
      ("mod", mod_float, Fpu.fmod, None, None);
    ];
  List.iter (speed_cmp1 n) [
      ("x^2", (fun x -> x**2.), Some (fun x -> Fpu.fpow x 2.),
       Cr.(fun x -> x**2), None);
    ];
  List.iter (speed_cmp1 n) [
      ("sin", sin, Some Fpu.fsin, Cr.sin, Some It.sin);
      ("cos", cos, Some Fpu.fcos, Cr.cos, Some It.cos);
      ("tan", tan, Some Fpu.ftan, Cr.tan, Some It.tan);
      ("exp", exp, Some Fpu.fexp, Cr.exp, Some It.exp);
      ("expm1", expm1, None, Cr.expm1, None);
    ];
  List.iter (speed_cmp1 ~pos:true n) [
      ("log",   log,   Some Fpu.flog, Cr.log,   Some It.log);
      ("log1p", log1p, None,          Cr.log1p, None);
    ]
