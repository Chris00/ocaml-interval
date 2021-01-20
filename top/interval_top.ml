open Printf

let eval_string
      ?(print_outcome = false) ?(err_formatter = Format.err_formatter) str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome err_formatter phrase

let load_interval() =
  let require pkg = eval_string (sprintf "#require %S;;" pkg) in
  require "interval_base"
  && require "interval_crlibm"
  && require "interval_intel"
  && eval_string "#install_printer Interval.Toploop.pp;;"


let () =
  if not (load_interval()) then
    Format.eprintf "Problem loading Interval libraries@."
