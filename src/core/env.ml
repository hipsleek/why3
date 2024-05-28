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

open Wstdlib

let debug_trace_env = Debug.register_info_flag "trace-env"
    ~desc:"Trace the execution of code in src/core/env.ml"

(** Library environment *)

type fformat = string (* format name *)
type filename = string (* file name *)
type extension = string (* file extension *)
type pathname = string list (* library path *)

exception KnownFormat of fformat
exception UnknownFormat of fformat
exception InvalidFormat of fformat
exception UnspecifiedFormat
exception UnknownExtension of extension

exception LibraryNotFound of pathname
exception TheoryNotFound of pathname * string
exception AmbiguousPath of filename * filename

type env = {
  env_path : Sstr.t;
  env_tag  : Weakhtbl.tag;
}

let env_tag env = env.env_tag

module Wenv = Weakhtbl.Make(struct type t = env let tag = env_tag end)

(** Environment construction and utilisation *)

let create_env = let c = ref (-1) in fun lp -> {
  env_path = Sstr.of_list lp;
  env_tag  = (incr c; Weakhtbl.create_tag !c)
}

let get_loadpath env = Sstr.elements env.env_path

(** Input languages *)

type 'a format_parser = env -> pathname -> filename -> in_channel -> 'a

type format_info = fformat * extension list * Pp.formatted

(* Hash table where key is path name *)
module Hpath = Exthtbl.Make(struct
  type t = pathname
  let hash = Hashtbl.hash
  let equal = (=)
end)

type 'a language = {
  memo : 'a Hpath.t Wenv.t;
  push : env -> pathname -> 'a -> unit;
  regf : format_info -> unit format_parser -> unit;
  regb : (env -> pathname -> unit) -> unit;
  mutable fmts : unit format_parser Mstr.t;
  mutable bins : (env -> pathname -> unit) list;
  mutable info : format_info list;
}

let base_language = {
  memo = Wenv.create 3;
  push = (fun _ _ _ -> ());
  regf = (fun _ _ -> ());
  regb = (fun _ -> ());
  fmts = Mstr.empty;
  bins = [];
  info = [];
}

exception LibraryConflict of pathname

let store lang env path c =
  (* get or create a new hash table *)
  let ht = try Wenv.find lang.memo env with Not_found ->
    let ht = Hpath.create 17 in Wenv.set lang.memo env ht; ht in
  match path with
  | "why3" :: _ ->
      begin try
        (* the cached value is different than the new value *)
        (* that means there is a conflict *)
        if Hpath.find ht path != c then raise (LibraryConflict path)
      (* if not found, it means that the value is not yet in the cache. Push it in the cache! *)
      with Not_found -> lang.push env path c; Hpath.add ht path c end
  | _ ->
      (* either path is empty, or path is not in the cache *)
      (* basically, if it's not why3 library, then it should be handled only once *)
      assert (path = [] || not (Hpath.mem ht path));
      lang.push env path c; Hpath.add ht path c

(* SHADOWED *)
let register_format lang (ff,_,_ as inf) fp =
  lang.regf inf fp;
  lang.fmts <- Mstr.add_new (KnownFormat ff) ff fp lang.fmts;
  lang.info <- inf :: lang.info

(* SHADOWED *)
let add_builtin lang bp =
  lang.regb bp;
  lang.bins <- bp :: lang.bins

let register_language parent convert = {
  (* register in the parent *)
  memo = Wenv.create 3;
  push = (fun env path c -> store parent env path (convert c));
  regf = (fun inf fp -> register_format parent inf fp);
  regb = (fun bp -> add_builtin parent bp);
  fmts = Mstr.empty;
  bins = [];
  info = [];
}

let extension_table = ref Mstr.empty

(* SHADOWING *)
let register_format ~desc lang ff extl fp =
  (* decorate the old parser *)
  (* store after the old parser returns *)
  Debug.dprintf debug_trace_env "register_format: ff=%s@." ff;
  let fp env path fn ch = store lang env path (fp env path fn ch) in
  register_format lang (ff,extl,desc) fp;
  (* also register the file format in the extension table *)
  let add_ext m e = Mstr.add e ff m in
  extension_table := List.fold_left add_ext !extension_table extl;
  Debug.dprintf debug_trace_env "register_format: extension_table=%s@." (String.concat ", " (Mstr.keys !extension_table))

(* SHADOWING *)
let add_builtin lang bp =
  let bp env path = store lang env ("why3" :: path) (bp path) in
  add_builtin lang bp

let list_formats lang =
  let filter_ext (ff,extl,desc) =
    let filt e = Mstr.find e !extension_table = ff in
    ff, List.filter filt extl, desc in
  List.rev_map filter_ext lang.info

(** Input file parsing *)

let get_extension file =
  let s = try Filename.chop_extension file
    with Invalid_argument _ -> raise UnspecifiedFormat in
  let n = String.length s + 1 in
  String.sub file n (String.length file - n)

let get_format ?format file =
  let ff = match format with
  | Some ff -> ff
  | None ->
      let ext = get_extension file in
      Mstr.find_exn (UnknownExtension ext) ext !extension_table
  in
  (* Debug.dprintf debug_trace_env "get_format: %s@." ff; *)
  ff

(* ff : string *)
let get_parser lang ff =
  (* Debug.dprintf debug_trace_env "get_parser: %s@." ff; *)
  try Mstr.find ff lang.fmts
  with Not_found ->
    if Mstr.mem ff base_language.fmts
      then raise (InvalidFormat ff)
      else raise (UnknownFormat ff)

let read_channel ?format lang env file ch =
  let ff = get_format ?format file in
  (* call the parser *)
  get_parser lang ff env [] file ch;
  Hpath.find (Wenv.find lang.memo env) []

let read_lib_file ?format lang env path file =
  (* file format *)
  let ff = get_format ?format file in
  (* get the parser based on the file format *)
  (* fp : unit format_parser *)
  let fp = get_parser lang ff in
  (* open the file *)
  let ch = open_in file in
  (* call the parser *)
  try fp env path file ch; close_in ch; ff
  (* propagate any error *)
  with exn -> close_in ch; raise exn

let read_file ?format lang env file =
  let format = read_lib_file ?format lang env [] file in
  (Hpath.find (Wenv.find lang.memo env) []), format

(** Library file parsing *)

let locate_library env path =
  if path = [] || path = ["why3"]
    then invalid_arg "Env.locate_library";
  let check_qualifier s =
    if (s = Filename.parent_dir_name ||
        s = Filename.current_dir_name ||
        Filename.basename s <> s)
    then invalid_arg "Env.locate_library" in
  List.iter check_qualifier path;
  let file = List.fold_left Filename.concat "" path in
  let add_ext ext = file ^ "." ^ ext in
  let fl = List.map add_ext (Mstr.keys !extension_table) in
  let add_dir dir = List.map (Filename.concat dir) fl in
  let fl = List.concat (List.map add_dir (get_loadpath env)) in
  match List.filter Sys.file_exists fl with
  | [] -> raise (LibraryNotFound path)
  | [file] -> file
  | file1 :: file2 :: _ -> raise (AmbiguousPath (file1, file2))

exception CircularDependency of pathname

(* SHADOWED *)
let read_library lang env = function
  | "why3" :: path ->
      (* bp : env -> pathname -> unit *)
      (* delegate to the handlers registered inside the language *)
      (* ignore error*)
      let read bp = try bp env path with Not_found -> () in
      List.iter read lang.bins
  | path ->
      (* locate the library, and read it *)
      let file = locate_library env path in
      let (_: fformat) = read_lib_file lang env path file in ()

let libstack = Hpath.create 17

(* SHADOWING, SHADOWED *)
let read_library lang env path =
  (* prevent circular dependency *)
  if Hpath.mem libstack path then
    raise (CircularDependency path);
  try
    Hpath.add libstack path ();
    read_library lang env path;
    Hpath.remove libstack path
  with exn ->
    Hpath.remove libstack path;
    raise exn

(* SHADOWING *)
let read_library lang env path =
  let path = if path = [] then ["why3"] else path in
  (* try read library from cache *)
  try Hpath.find (Wenv.find lang.memo env) path with Not_found ->
  (* if fail, try to read library from file *)
  (* implemented above *)
  read_library lang env path;
  (* try read again *)
  try Hpath.find (Wenv.find lang.memo env) path with Not_found ->
  (* if still fail, then now it must be case that the library does not exist *)
  raise (LibraryNotFound path)

let read_theory env path s =
  let path = if path = [] then ["why3"; s] else path in
  let mt = read_library base_language env path in
  Mstr.find_exn (TheoryNotFound (path,s)) s mt

(* Builtin theories *)

open Ident
open Theory

let base_language_builtin =
  (* builtin : string -> theory *)
  let builtin s =
    if s = builtin_theory.th_name.id_string then builtin_theory else
    if s = ignore_theory.th_name.id_string then ignore_theory else
    if s = bool_theory.th_name.id_string then bool_theory else
    if s = highord_theory.th_name.id_string then highord_theory else
    match tuple_theory_name s with
    | Some n -> tuple_theory n
    | None -> raise Not_found
  in
  Hpath.memo 7 (function
    | [s] -> Mstr.singleton s (builtin s)
    | _   -> raise Not_found)

(* add builtin theory for the base language *)
let () = add_builtin base_language base_language_builtin

(* Exception reporting *)

let print_path fmt sl =
  Pp.print_list (Pp.constant_string ".") Format.pp_print_string fmt sl

let () = Exn_printer.register
  begin fun fmt exn -> match exn with
  | CircularDependency sl -> Format.fprintf fmt
      "Circular dependency in %a" print_path sl
  | LibraryNotFound sl -> Format.fprintf fmt
      "Library file not found: %a" print_path sl
  | TheoryNotFound (sl,s) -> Format.fprintf fmt
      "Theory %s not found in library %a" s print_path sl
  | KnownFormat s -> Format.fprintf fmt
      "Format %s is already registered" s
  | UnknownFormat s -> Format.fprintf fmt
      "Unknown input format: %s" s
  | UnknownExtension s -> Format.fprintf fmt
      "Unknown file extension: `%s'" s
  | UnspecifiedFormat -> Format.fprintf fmt
      "Format not specified"
  | AmbiguousPath (f1,f2) -> Format.fprintf fmt
      "Ambiguous path:@ both %s@ and %s@ match" f1 f2
  | InvalidFormat f -> Format.fprintf fmt
      "Input format `%s' is unsuitable for the desired content" f
  | LibraryConflict sl -> Format.fprintf fmt
      "Conflicting definitions for builtin library %a" print_path sl
  | _ -> raise exn
  end
