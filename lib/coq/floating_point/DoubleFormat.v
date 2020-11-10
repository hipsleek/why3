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

(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.

(* Why3 goal *)
Definition any_function {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b} :
  a -> b.
Proof.

Defined.

Require Import floating_point.GenFloat.

(* Why3 goal *)
Definition double : Type.
exact (t 53 1024).
Defined.

Global Instance double_WhyType : WhyType double.
Proof.
apply t_WhyType.
Qed.
