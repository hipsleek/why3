(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2023 --  Inria - CNRS - Paris-Saclay University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

open Why3
open Why3session_lib
open Session_itp

let usage_msg =
  "<files>\n\
  \ Generate a fresh session containing the specified files."

let provers = ref []
let opt_outputdir = ref None
let opt_timelimit = ref None
let opt_stepslimit = ref None
let opt_memlimit = ref None

let spec =
  let open Getopt in
  [
    ( Key ('P', "provers"),
      Hnd1 (AList (':', AString), fun p -> provers := p),
      "<prover1:prover2...> Specify provers" );
    ( Key ('o', "output-dir"),
      Hnd1 (AString, fun s -> opt_outputdir := Some s),
      "<dir> set the output dir" );
    ( Key ('t', "timelimit"),
      Hnd1 (AFloat, fun i -> opt_timelimit := Some i),
      "<sec> set the prover's time limit" );
    ( Key ('s', "stepslimit"),
      Hnd1 (AInt, fun i -> opt_stepslimit := Some i),
      "<steps> set the prover's step limit (no limit by default)" );
    ( Key ('m', "memlimit"),
      Hnd1 (AInt, fun i -> opt_memlimit := Some i),
      "<MB> set the prover's memlimit" );
  ]


open Format

let run () =
  let config, env = Whyconf.Args.complete_initialization () in
  let dir =
    match !opt_outputdir with
    | None ->
        eprintf "option -o is mandatory@.";
        Whyconf.Args.exit_with_usage usage_msg
    | Some f ->
        (* for the moment, we forbid to exend an already existing session *)
        if Sys.file_exists (Sysutil.concat f "why3session.xml") then
          begin
            (* FIXME: allow to extend an existing session *)
            eprintf "session file already exist, aborting.@.";
            Whyconf.Args.exit_with_usage usage_msg
          end;
        let q = Queue.create () in
        Queue.push f q;
        try Server_utils.get_session_dir ~allow_mkdir:true q with
        | Invalid_argument s ->
            eprintf "Error: %s@." s;
            Whyconf.Args.exit_with_usage usage_msg
  in
  let main = Whyconf.get_main config in
  let limit =
    {
      Call_provers.limit_time =
        (match !opt_timelimit with
         | None -> Whyconf.timelimit main
         | Some l -> l);
      Call_provers.limit_steps =
        (match !opt_stepslimit with
         | None -> 0
         | Some l -> l);
      limit_mem =
        (match !opt_memlimit with
         | None -> Whyconf.memlimit main
         | Some l -> l);
    }
  in
  (* loading the drivers *)
  let provers =
    List.map
      (fun p ->
         let filter_prover = Whyconf.parse_filter_prover p in
         Whyconf.filter_one_prover config filter_prover)
      !provers
  in
  let session = load_session dir in
  let errors = ref [] in
  let add_file_and_vcs_to_session f =
    let f = Sysutil.concat (Sys.getcwd ()) f in
    let file_is_detached,(theories,format) =
      try false,(Session_itp.read_file env f)
      with e -> errors := e :: !errors; true,([], Env.get_format f)
    in
    let file = add_file_section ~file_is_detached session f theories format in
    let theories = file_theories file in

    (* Apply split_vc on goal then call each specified prover on the generated
         subgoals *)
    let add_proof_attempt g =
      let l =
        apply_trans_to_goal ~allow_no_effect:true session env "split_vc" [] g
      in
      let tsid = graft_transf session g "split_vc" [] l in
      List.iter
        (fun g ->
           List.iter
             (fun p ->
                let _ = graft_proof_attempt session g p.Whyconf.prover ~limit in
                ())
             provers)
        (get_sub_tasks session tsid)
    in
    List.iter
      (fun th -> List.iter add_proof_attempt (theory_goals th))
      theories
  in
  (* FIXME: if the file is already present in the session, a new
     duplicate file node is added. We would prefer to modify the
     existing one *)
  iter_files add_file_and_vcs_to_session;
  save_session session;
  match !errors with
  | [] -> eprintf "Session created successfully@."
  | _ ->
      eprintf "Session created but with the following errors:@.";
      List.iter (fun e -> eprintf "%a@." Exn_printer.exn_printer e) !errors


let cmd =
  {
    cmd_spec = spec;
    cmd_desc = "Create a session";
    cmd_name = "create";
    cmd_run = run;
  }