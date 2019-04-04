(*
    Copyright 2011 Jean-Marc Alliot / Jean-Baptiste Gotteland

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

open Printf

let rnd_values = Array.init 1000 (fun _ -> 1000. -. Random.float 2000.)

let speed_cmp1 loops (name, f) =
  let l = Array.length rnd_values in
  printf "%10s speed (%d calls): %!" name loops;
  let top = (Unix.times ()).Unix.tms_utime in
  for _ = 1 to loops / l do
    Array.iter (fun x -> ignore (f x)) rnd_values
  done;
  let dt = (Unix.times ()).Unix.tms_utime -. top in
  printf "%f\n%!" dt

let speed_cmp2 loops (name, f) =
  let l = Array.length rnd_values in
  printf "%10s speed (%d calls): %!" name loops;
  let top = (Unix.times ()).Unix.tms_utime in
  for _ = 1 to loops / l do
    Array.iter (fun x -> ignore (f x x)) rnd_values;
  done;
  let dt = (Unix.times ()).Unix.tms_utime -. top in
  printf "%f\n%!" dt


type test_mode = Exact | In | Mod2pi

type test_infos = {
    mode: test_mode;
    res_I: Interval.t;
    msg: string;
    mutable res_low: float;
    mutable res_high: float;
    mutable args_low: float list;
    mutable args_high: float list;
    mutable inf: bool;
    mutable neg_inf: bool;
  }

module type FLOAT_INTERVAL = sig
  include Interval.T with type number = float and type t = Interval.t
  val log : t -> t
  val exp : t -> t
  val cos : t -> t
  val sin : t -> t
  val tan : t -> t
  val acos : t -> t
  val asin : t -> t
  val atan : t -> t
  val cosh : t -> t
  val sinh : t -> t
  val tanh : t -> t
end

module type DIRECTED = sig
  include Interval.DIRECTED with type t = float
  val exp : t -> t
  val log : t -> t
  val cos : t -> t
  val sin : t -> t
  val tan : t -> t
  val asin : t -> t
  val acos : t -> t
  val atan : t -> t
  val cosh : t -> t
  val sinh : t -> t
  val tanh : t -> t
end

module Test (I: FLOAT_INTERVAL) (Low: DIRECTED) (High: DIRECTED) = struct

  let create_test mode f x =
    flush stdout;
    let (res_I, msg) = try (f x, "")
                       with err -> (I.zero, Printexc.to_string err) in
    print_endline (if msg = "" then I.to_string res_I ~fmt:"%.25e" else msg);
  { mode = mode;
    res_I = res_I;
    msg = msg;
    res_low = infinity;
    res_high = neg_infinity;
    args_low = [];
    args_high = [];
    inf = false;
    neg_inf = false; }

  let test_I {Interval.low = a; high = b} =
    a = a && b = b && a <= b && a <> infinity && b <> neg_infinity

  let add_result infos args res =
    if res = res then (
      let resm =
        if infos.mode <> Mod2pi || infos.msg <> "" || infos.res_I.low <= res then
          res
        else (res +. I.two_pi.high) in
      if resm = infinity then (
        infos.inf <- true;
        infos.args_high <- args)
      else if resm = neg_infinity then (
        infos.neg_inf <- true;
        infos.args_low <- args)
      else (
        if resm < infos.res_low then (
          infos.res_low <- resm;
          infos.args_low <- args);
        if infos.res_high < resm then (
          infos.res_high <- resm;
          infos.args_high <- args)))

  let print_test infos =
    let error msg =
      if infos.res_low <= infos.res_high then
        printf "%s %s\nDEFINED\nArgs low:  %s\nArgs high: %s\n%!" msg
          (I.to_string ~fmt:"%.25e" (I.v infos.res_low infos.res_high))
          (String.concat " " (List.map (sprintf "%.25e") infos.args_low))
          (String.concat " " (List.map (sprintf "%.25e") infos.args_high))
      else print_endline msg;
      (* ignore (input_line stdin) *)
    in
    if infos.neg_inf && (
      infos.res_low = -.max_float || infos.res_low < infos.res_high) then
      infos.res_low <- neg_infinity;
    if infos.inf && (
      infos.res_high = max_float || infos.res_low < infos.res_high) then
      infos.res_high <- infinity;
    if infos.msg = "" then (
      if infos.res_low <= infos.res_high then (
        if not (test_I infos.res_I) then error "Interval not valid.\nShould be"
        else if infos.res_low < infos.res_I.low ||
                  infos.res_I.high < infos.res_high then error "Should_contain"
        else if infos.mode = Exact &&
                  (infos.res_low <> infos.res_I.low ||
                     infos.res_high <> infos.res_I.high) then error "Should be")
      else error "Should fail.")
    else if infos.res_low <= infos.res_high then error "Should be"

  let rec iter f = function
    | a::tl as l when a <> infinity ->
       let b_list = if a = neg_infinity then tl else l in
       List.iter (fun b -> f (I.v a b)) b_list;
       iter f tl
    | _ -> ()

  let iter_in x_I f values =
    let open Interval in
    List.iter (fun x ->
        if x_I.low <= x && x <= x_I.high then
          if x = 0. then (
            if x_I.low < 0. then f (-0.);
            if 0. < x_I.high || x_I.low = 0. then f 0.)
          else f x)
      (x_I.low :: x_I.high :: values)

  let iter_finite f values =
    List.iter (fun x -> if classify_float x <> FP_infinite then f x) values

  let check_I mode values bounds (name, f_list, f_I) =
    iter (fun x_I ->
        printf "%s: %s =\n" name (I.to_string ~fmt:"%.25e" x_I);
        let infos = create_test mode f_I x_I in
        iter_in x_I (fun x ->
            List.iter (fun f -> add_result infos [x] (f x)) f_list) values;
        print_test infos) bounds

  let check_I_f mode values bounds (name, f_list, f_I_f) =
    iter (fun x_I -> iter_finite (fun y ->
      printf "%s: %s %e =\n" name (I.to_string ~fmt:"%e" x_I) y;
      let infos = create_test mode (f_I_f x_I) y in
      iter_in x_I (fun x ->
          if name = "pow" && x = 0. then
            printf "%e ^ %e = %e\n%!" x y (List.hd f_list x y);
          List.iter (fun f -> add_result infos [x; y] (f x y)) f_list) values;
      print_test infos) bounds) bounds

  let check_f_I mode values bounds (name, f_list, f_f_I) =
    iter_finite (fun x -> iter (fun y_I ->
      printf "%s: %e %s =\n" name x (I.to_string ~fmt:"%e" y_I);
      let infos = create_test mode (f_f_I x) y_I in
      iter_in y_I (fun y ->
          List.iter (fun f -> add_result infos [x; y] (f x y)) f_list) values;
      print_test infos) bounds) bounds

  let check_I_I mode values bounds (name, f_list, f_I_I) =
    iter (fun x_I -> iter (fun y_I ->
            printf "%s: %s %s =\n" name (I.to_string ~fmt:"%e" x_I)
            (I.to_string ~fmt:"%e" y_I);
      let infos = create_test mode (f_I_I x_I) y_I in
      iter_in x_I  (fun x -> iter_in y_I (fun y ->
        List.iter (fun f -> add_result infos [x; y] (f x y)) f_list)
                               values) values;
      print_test infos) bounds) bounds

  let check_I_i mode values bounds valuesi (name, f_list, f_I_i) =
    iter (fun x_I -> List.iter (fun n ->
      printf "%s: %s %d =\n" name (I.to_string ~fmt:"%e" x_I) n;
      let infos = create_test mode (f_I_i x_I) n in
      let ny = float n in
      iter_in x_I (fun x ->
          List.iter (fun f -> add_result infos [x; ny] (f x ny)) f_list) values;
      print_test infos) valuesi) bounds

  let rnd_values_I =
    Array.init 1000 (fun _ ->
        let x1 = 1000. -. Random.float 2000. and x2 = Random.float 10. in
        I.v x1 (x1 +. x2))

  let rnd_values_pos_I =
    Array.init 1000 (fun _ ->
        let x1 = Random.float 2000. and x2 = Random.float 10. in
        I.v x1 (x1 +. x2))

  let speed_cmp1_I ?(pos=false) loops (name, f) =
    let l = Array.length rnd_values_I in
    printf "%10s speed (%d calls): %!" name loops;
    let rnd_I = if pos then rnd_values_pos_I else rnd_values_I in
    let top = (Unix.times ()).Unix.tms_utime in
    for _ = 1 to loops / l do
      Array.iter (fun x -> ignore (f x)) rnd_I
    done;
    let dt = (Unix.times ()).Unix.tms_utime -. top in
    printf "%f\n%!" dt

  let speed_cmp2_I loops (name, f) =
    let l = Array.length rnd_values_I in
    printf "%10s speed (%d calls): %!" name loops;
    let top = (Unix.times ()).Unix.tms_utime in
    for _ = 1 to loops / l do
      Array.iter (fun x -> ignore (f x x)) rnd_values_I
    done;
    let dt = (Unix.times ()).Unix.tms_utime -. top in
    printf "%f\n%!" dt

  let inv x = 1. /. x
  let inv_low x = Low.(1. /. x)
  let inv_high x = High.(1. /. x)

  let o3 = 1./.3. and o7 = 1./.7. and e = exp 1.
  let m1 = 1. -. epsilon_float and p1 = 1. +. epsilon_float
  let values = [-.max_float; -.10.; -.1.; -.o7; 0.; o7; 1.; 10.; max_float]
  let bounds = [neg_infinity; -.max_float /.2.; -.e; -.p1; -1.; -.m1; -.o3;
                0.; o3; m1; 1.; p1; e; max_float /.2.; infinity]

    let () =
    let top = Sys.time () in
    let da = acos (1. -. epsilon_float) in
    let rec add_mul al ah da i j l0 =
      if i <= j then float i *. al -. da
                  :: float i *. ah +. da
                  :: add_mul al ah da (i + 1) j l0
    else l0 in
    let pio2s =
      add_mul I.half_pi.low I.half_pi.high 0. (-1010) (-990)
        (add_mul I.half_pi.low I.half_pi.high 0. (-10) 10
           (add_mul I.half_pi.low I.half_pi.high 0. 990 1010 [])) in
    let angles =
      neg_infinity::
        add_mul I.half_pi.low I.half_pi.high da (-1006) (-994)
          (add_mul I.half_pi.low I.half_pi.high da (-6) 6
             (add_mul I.half_pi.low I.half_pi.high da 994 1006 [infinity])) in

    List.iter (check_I_f Exact values bounds)
      [ ("I + f", [( +. ); Low.( +. ); High.( +. )], I.( +. ));
        ("I - f", [( -. ); Low.( -. ); High.( -. )], I.( -. ));
        ("I * f", [( *. ); Low.( *. ); High.( *. )], I.( *: ));
        ("I / f", [( /. ); Low.( /. ); High.( /. )], I.( /. )) ];

  List.iter (check_I Exact values bounds)
    [ ("I.abs",  [abs_float], I.abs);
      ("I.inv",  [inv; inv_low; inv_high], I.inv);
      ("I.log",  [log; Low.log; High.log], I.log);
      ("I.exp",  [exp; Low.exp; High.exp], I.exp);
      ("I.atan", [atan; Low.atan; High.atan], I.atan);
      ("I.asin", [asin; Low.asin; High.asin], I.asin);
      ("I.acos", [acos; Low.acos; High.acos], I.acos);
      ("I.cosh", [cosh; Low.cosh; High.cosh], I.cosh);
      ("I.sinh", [sinh; Low.sinh; High.sinh], I.sinh);
      ("I.tanh", [tanh; Low.tanh; High.tanh], I.tanh); ];

  List.iter (check_I Exact pio2s angles)
    [ ("I.cos", [cos; Low.cos; High.cos], I.cos);
      ("I.sin", [sin; Low.sin; High.sin], I.sin)];
  check_I In pio2s angles ("I.tan", [tan; Low.tan; High.tan], I.tan);

  List.iter (check_f_I Exact values bounds)
    [ ("f + I", [( +. ); Low.( +. ); High.( +. )], I.( +: ));
      ("f - I", [( -. ); Low.( -. ); High.( -. )], I.( -: ));
      ("f * I", [( *. ); Low.( *. ); High.( *. )], I.( *. ));
      ("f / I", [( /. ); Low.( /. ); High.( /. )], I.( /: )) ];

  List.iter (check_I_I Exact values bounds)
    [ ("I + I", [( +. ); Low.( +. ); High.( +. )], I.( + ));
      ("I - I", [( -. ); Low.( -. ); High.( -. )], I.( - ));
      ("I * I", [( *. ); Low.( *. ); High.( *. )], I.( * ));
      ("I / I", [( /. ); Low.( /. ); High.( /. )], I.( / ));
      ("max I I", [ max; max; max], I.max);
      ("min I I", [ min; min; min], I.min)];

  printf "%f seconds.\n%!" (Sys.time () -. top);

  List.iter (speed_cmp1 10000000) [
      ("tan", tan); ("cos", cos); ("sin", sin);
      ("exp", exp); ("log", log) ];

  List.iter (speed_cmp1_I 10000000) [
      ("I.tan", I.tan); ("I.cos", I.cos); ("I.sin", I.sin);
      ("I.exp", I.exp); ];
  List.iter (speed_cmp1_I 10000000 ~pos:true) [
      ("I.log", I.log) ];

  List.iter (speed_cmp2 10000000) [
      ("+.", ( +. )); ("-.", ( -. )); ("*.", ( *. )); ("/.", ( /. ));
      ("**", ( ** )); ("mod_float", mod_float)];

  List.iter (speed_cmp2_I 20000000) [
      ("I+I", I.( + )); ("I-I", I.( - ));
      ("I*I", I.( * )); ("I/I", I.( / ))];
end


module Test_Intel = struct
  let () =
    printf "*** Test Interval_intel ***\n%!"

  open Interval_intel
  module I = Interval_intel.I
  module Fpu = Interval_intel.Fpu
  include Test(I)(Interval_intel.Low)(Interval_intel.High)

  let myatan2 y x = if y = 0.&& x = 0. then nan else atan2 y x
  let myatan2_low y x = if y = 0. && x = 0. then nan else Low.atan2 y x
  let myatan2_high y x = if y = 0. && x = 0. then nan else High.atan2 y x

  let pospow x y =
    if x = 0. then Fpu.fpow 0. y
    else if x = infinity || y = infinity || y = neg_infinity then Fpu.fpow x y
    else Fpu.flog_pow x y

  let valuesi =  [min_int; -1000; -1; 0; 1; 10; 1000; 1073741823]

  let () =
    check_I_f Exact values bounds
      ("I**f", [Fpu.fpow; Low.( ** ); High.( ** )], I.( **. ));
    check_f_I Exact values bounds
      ("f**I", [pospow; Low.pow; High.pow], I.( **: ));
    check_I Exact values bounds
      ("I.sqrt", [sqrt; Low.sqrt; High.sqrt], I.sqrt);
    check_I_f In values bounds ("I.mod_f", [Fpu.fmod; mod_float], I.mod_f);
    check_I_i Exact values bounds valuesi
      ("I.( ** )", [Fpu.fpow; Low.( ** ); High.( ** )], I.( ** ));
    List.iter (check_I_I Exact values bounds)
      [ ("I.atan2", [myatan2; myatan2_low; myatan2_high], I.atan2);
        ("I**I", [pospow; Low.pow; High.pow], I.( *** ) ) ];
    check_I_I Mod2pi values bounds
      ("I.atan2mod", [myatan2; myatan2_low; myatan2_high], I.atan2mod);

    List.iter (speed_cmp1 10000000) [
        ("ftan", Fpu.ftan); ("fcos", Fpu.fcos); ("fsin", Fpu.fsin);
        ("fexp", Fpu.fexp); ("flog", Fpu.flog) ];
    List.iter (speed_cmp2 10000000) [
        ("fadd", Fpu.fadd); ("fsub", Fpu.fsub);
        ("fmul", Fpu.fmul); ("fdiv", Fpu.fdiv);
        ("fpow", Fpu.fpow);
        ("fmod", Fpu.fmod)];
end

module Test_Crlibm = struct
  let () =
    printf "*** Test Interval_crlibm ***\n%!"

  module I = Interval_crlibm.I
  include Test(I)(Interval_crlibm.Low)(Interval_crlibm.High)
end
