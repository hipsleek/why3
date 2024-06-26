open Format
open Why3

let usage_msg = "<file>\nParse the given file, then display the AST in Sexp format"
let opt_file = ref None
let option_list = []

let add_opt_file file = opt_file := Some file

let _, _ =
  eprintf "why3pp_sleek.ml: initialize args@.";
  Whyconf.Args.initialize option_list add_opt_file usage_msg

let parse_mlw_file filename =
  let c = if filename = "-" then stdin else open_in filename in
  let lexbuf = Lexing.from_channel c in
  Loc.set_file filename lexbuf;
  let mlw_file = Lexer_sleek.parse_mlw_file lexbuf in
  if filename <> "-" then
    close_in c;
  mlw_file
let handle_no_file () =
  Whyconf.Args.exit_with_usage usage_msg

let handle_file file =
  let mlw_file = parse_mlw_file file in
  let sexp = Why3.Ptree_sleek.sexp_of_mlw_file mlw_file in
  Mysexplib.output std_formatter sexp

let () =
  eprintf "why3pp_sleek: handle input file@.";
  try
    match !opt_file with
      | None -> handle_no_file ()
      | Some file -> handle_file file
  with e when not (Debug.test_flag Debug.stack_trace) ->
    eprintf "%a@." Exn_printer.exn_printer e;
    exit 1
