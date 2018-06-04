(* Dispatch the files and perform the substitutions to share files
   among the various sub-libraries. *)

open Stdio

let crlibm () =
  let source = "../src-intel/generic.ml" in (* from root *)
  let s = In_channel.read_all source in
  let lu_re = Str.regexp "Fpu\\.\\(Low\\|High\\)" in
  let s = Str.global_replace lu_re "Crlibm.\\1" s in
  (* Assume [mod_float x 2.] is exact — it is easy enough to implement... *)
  let s = Str.global_replace (Str.regexp "Fpu\\.fmod") "mod_float" s in
  Out_channel.with_file "generic.ml" ~f:(fun fh ->
      Out_channel.fprintf fh "# 1 %S\n" ("src-crlibm/" ^ source);
      Out_channel.output_string fh s
    )

let () =
  crlibm()
