(* what do we need for forward verification? *)
(* we need a proof environment *)
(* to build the proof environment, we shall traverse the parse tree and augment it on the fly *)
(* we can then translate this proof enviroment into sleek context using the API *)

(* what are the information that we need to store in the proof enviroment? *)
(* DO: *)
(* we shall store top-level lemmas that are declared in the file. Just traverse the tree once, while
   collecting the lemmas into the environment *)
(* for each record type definition, we shall encode that record declaration into a sleek data declarion *)
(* for each inductive predicate definition (should be stored in Ptree_sleek.Dsleek),
   we shall feed it into the API and store the returned inductive predicate into the enviroment *)
(* specifically handle option: None is just null, and Just is the content itself *)
(* for each top-level function, we need to store its pre-condition and post-condition to generate proof goals later
   in the proof for the function call and augment the proof enviroment / context during the next step of the proof *)

(* DON'T *)
(* we shall not handle ADT at the moment *)

(*
val add_predicate : 'a -> 'b
val add_data : 'a -> 'b
val add_lemma : 'a -> 'b
val add_spec : Ptree_sleek.mlw_file -> 'a
val to_sleek_context : proof_env -> 'a
*)


(* goal : first try to make the example x >= 0 -> x >= 1 works *)
(* how to do verification? traverse the parse tree twice:
   - the first traversal is to build the basic environment
   - the second traversal is to reduce the parse tree into their core language
     and then we shall traverse on this core language instead
 *)

open Wstdlib
open Hipsleek_api

val gather_spec_in_mlw_file : Ptree_sleek.mlw_file -> string list Mstr.t

val gather_data_decl_in_mlw_file : Ptree_sleek.mlw_file -> unit

val gather_logic_decl_in_mlw_file : Ptree_sleek.mlw_file -> unit

type spec_map = (Sleekapi.sf * Sleekapi.param list) Mstr.t

val compile_spec_in_pmodule : string list Mstr.t -> Pmodule.pmodule -> spec_map

val verify_function : spec_map -> Pmodule.mod_unit -> bool

val verify_module : spec_map -> Pmodule.pmodule -> bool
