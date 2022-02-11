(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2021 --  Inria - CNRS - Paris-Saclay University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

open Wstdlib
open Smtv2_model_defs


(* From the table generated by the parser, build a raw model (i.e. list of model_element) *)
val create_list : Printer.printing_info -> definition Mstr.t ->
  Model_parser.model_value Mstr.t
