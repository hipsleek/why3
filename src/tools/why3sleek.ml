open Format
open Why3
open Wstdlib
open Hipsleek_api

let usage_msg =
  "<file>\n\
   Parse the given file, then display the AST in Sexp format or prove it."

let opt_prove = ref false
let opt_file = ref None
let option_list =
  let open Getopt in
  [ Key ('p', "prove"), Hnd0 (fun () -> opt_prove := true),
    "proving instead of prining"
  ]

let add_opt_file file = opt_file := Some file

let _, env =
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

let prove_file file mlw_file =
  let mlw_file_no_sleek = Ptree_sleek_helpers.drop_sleek_in_mlw_file mlw_file in
  let pmodules = Typing.type_mlw_file env [] file mlw_file_no_sleek in
  let pmodules = Wstdlib.Mstr.values pmodules in
  match pmodules with
    | [pmod] ->
        let open Forward_verification in
        let built_in_specs = Sleek_primitives.register_primitives () in
        Format.eprintf "why3sleek: primitives registered@.";
        gather_data_decl_in_mlw_file mlw_file;
        gather_logic_decl_in_mlw_file mlw_file;
        Format.eprintf "why3sleek: top-level declarations registered@.";
        let specs = gather_spec_in_mlw_file mlw_file in
        let specs = compile_spec_in_pmodule specs pmod in
        let specs = Mstr.set_union built_in_specs specs in
        let ok = verify_module specs pmod in
        printf "prover returned: %s@." (string_of_bool ok)
    | _ -> raise (Invalid_argument "only support file with exactly one module!")

let print_file mlw_file =
  let sexp = Why3.Ptree_sleek.sexp_of_mlw_file mlw_file in
  Mysexplib.output std_formatter sexp

let handle_file file =
  let mlw_file = parse_mlw_file file in
  if !opt_prove then
    prove_file file mlw_file
  else
    print_file mlw_file

let () = Sleekapi.init ()

let () =
  eprintf "why3sleek: handle input file@.";
  try
    match !opt_file with
      | None -> handle_no_file ()
      | Some file -> handle_file file
  with e when not (Debug.test_flag Debug.stack_trace) ->
    eprintf "%a@." Exn_printer.exn_printer e;
    exit 1
