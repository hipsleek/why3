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

val parse_term : Lexing.lexbuf -> Ptree_sleek.term

val parse_expr : Lexing.lexbuf -> Ptree_sleek.expr

val parse_decl : Lexing.lexbuf -> Ptree_sleek.decl

val parse_term_list: Lexing.lexbuf -> Ptree_sleek.term list

val parse_qualid: Lexing.lexbuf -> Ptree_sleek.qualid

val parse_list_qualid: Lexing.lexbuf -> Ptree_sleek.qualid list

val parse_list_ident: Lexing.lexbuf -> Ptree_sleek.ident list

(* TODO: mlws file instead of mlw file *)
val parse_mlw_file: Lexing.lexbuf -> Ptree_sleek.mlw_file

(* TODO: no longer return Pmodule.mlw_file *)
(* val read_channel: Env.env -> Env.pathname -> string -> in_channel ->
  Pmodule.mlw_file *)

(* Name of the registered format for whyml *)
val whyml_sleek_format: Env.fformat
