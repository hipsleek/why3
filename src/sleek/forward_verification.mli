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

type proof_env

(* these are functions that modify the proof environment *)
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
