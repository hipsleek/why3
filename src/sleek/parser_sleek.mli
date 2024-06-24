
(* The type of tokens. *)

type token = Parser_sleek_tokens.token

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val term_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ptree_sleek.term)

val term_comma_list_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ptree_sleek.term list)

val qualid_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ptree_sleek.qualid)

val qualid_comma_list_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ptree_sleek.qualid list)

val mlw_file_parsing_only: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ptree_sleek.mlw_file)

val mlw_file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Pmodule.pmodule Wstdlib.Mstr.t)

val ident_comma_list_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ptree_sleek.ident list)

val expr_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ptree_sleek.expr)

val decl_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ptree_sleek.decl)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val term_eof: Lexing.position -> (Ptree_sleek.term) MenhirInterpreter.checkpoint
  
  val term_comma_list_eof: Lexing.position -> (Ptree_sleek.term list) MenhirInterpreter.checkpoint
  
  val qualid_eof: Lexing.position -> (Ptree_sleek.qualid) MenhirInterpreter.checkpoint
  
  val qualid_comma_list_eof: Lexing.position -> (Ptree_sleek.qualid list) MenhirInterpreter.checkpoint
  
  val mlw_file_parsing_only: Lexing.position -> (Ptree_sleek.mlw_file) MenhirInterpreter.checkpoint
  
  val mlw_file: Lexing.position -> (Pmodule.pmodule Wstdlib.Mstr.t) MenhirInterpreter.checkpoint
  
  val ident_comma_list_eof: Lexing.position -> (Ptree_sleek.ident list) MenhirInterpreter.checkpoint
  
  val expr_eof: Lexing.position -> (Ptree_sleek.expr) MenhirInterpreter.checkpoint
  
  val decl_eof: Lexing.position -> (Ptree_sleek.decl) MenhirInterpreter.checkpoint
  
end
