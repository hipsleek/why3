(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2020   --   Inria - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

open Format
open Why3
open Whyconf

let conf_file = ref None
let show_config = ref false

(* When no arguments are given, activate the fallback to auto mode on error.
   true <-> fallback *)
let auto_fb = Array.length Sys.argv = 1

let opt_list_binaries = ref false

let save = ref true

let set_oref r = (fun s -> r := Some s)

let prover_bins = Queue.create ()

let plugins = Queue.create ()
let add_plugin x = Queue.add x plugins

let spec =
  let open Getopt in
  [ Key ('C', "config"), Hnd1 (AString, set_oref conf_file),
    "<file> config file to create";
    KLong "add-prover", Hnd1 (APair (',', AString, (APair (',', AString, AString))),
                              fun (same_as, (shortcut, binary)) ->
                                Queue.add {Autodetection.Manual_binary.same_as;
                                           shortcut;
                                           binary } prover_bins),
    "<id>,<shortcut>,<file> add a new prover executable as if it was id";
    KLong "show-config", Hnd0 (fun () -> show_config := true),
    " show the expansion of the configuration";
    KLong "list-binaries", Hnd0 (fun () -> opt_list_binaries := true),
    " list name of binaries looked for";
    KLong "install-plugin", Hnd1 (AString, add_plugin),
    "<file> copy a plugin to the current library directory";
    KLong "dont-save", Hnd0 (fun () -> save := false),
    " do not modify the config file";
    Debug.Args.desc_debug;
    Debug.Args.desc_debug_all;
    Debug.Args.desc_debug_list;
  ]

let usage () =
  Printf.eprintf
    "Usage: %s [options] \n\
     Detect provers to configure Why3.\n\
     \n%s%!"
    (Filename.basename Sys.argv.(0))
    (Getopt.format spec);
  exit 0

let spec =
  let open Why3.Getopt in
  (Key ('h', "help"), Hnd0 usage," display this help and exit") :: spec

let anon_file x = raise (Getopt.GetoptFailure (sprintf "unexpected argument: %s" x))

(* let add_prover_binary config (id,shortcut,file) =
 *   Autodetection.add_prover_binary config id shortcut file *)

let main () =
  (* Parse the command line *)
  Getopt.parse_all ~i:!Whyconf.Args.first_arg spec anon_file Sys.argv;

  let opt_list = ref false in

  (* Debug flag *)
  Debug.Args.set_flags_selected ();

  if !opt_list_binaries then begin
    opt_list := true;
    printf "@[<hov 2>Binaries looked for:@\n%a@]@\n@."
      (Pp.print_list Pp.newline Pp.string)
      (List.sort String.compare (Autodetection.list_binaries ()))
  end;

  opt_list := Debug.Args.option_list () || !opt_list;
  if !opt_list then exit 0;

  (* Main *)

  if !show_config then begin
    let config = Whyconf.init_config !conf_file in
    let rc = Whyconf.rc_of_config config in
    Rc.to_channel stdout rc
  end else begin
    Autodetection.is_config_command := true;
    let config =
      try
        Whyconf.read_config !conf_file
      with
      | Rc.CannotOpen (f, s)
      | Whyconf.ConfigFailure (f, s) ->
          eprintf "warning: cannot read config file %s:@\n  %s@." f s;
          Whyconf.default_config f
    in

    let config = Queue.fold Autodetection.Manual_binary.add config prover_bins in

    let config =
      let env = Autodetection.run_auto_detection config in
      Autodetection.generate_detected_config env config
    in
    if !save then begin
      printf "Save config to %s@." (Whyconf.get_conf_file config);
      save_config config
    end
  end

let () =
  try
    main ()
  with e when not (Debug.test_flag Debug.stack_trace) ->
    eprintf "Error: %a@." Exn_printer.exn_printer e;
    exit 1
