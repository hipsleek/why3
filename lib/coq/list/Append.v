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
Require int.Int.
Require list.List.
Require list.Length.
Require list.Mem.

(* Why3 goal *)
Definition any_function {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b} :
  a -> b.
Proof.

Defined.

(* Why3 goal *)
Lemma infix_plpl'def {a:Type} {a_WT:WhyType a} :
  forall (l1:Init.Datatypes.list a) (l2:Init.Datatypes.list a),
  ((Init.Datatypes.app l1 l2) =
   match l1 with
   | Init.Datatypes.nil => l2
   | Init.Datatypes.cons x1 r1 =>
       Init.Datatypes.cons x1 (Init.Datatypes.app r1 l2)
   end).
Proof.
now intros [|h1 q1] l2.
Qed.

Require Import Lists.List.

(* Why3 goal *)
Lemma Append_assoc {a:Type} {a_WT:WhyType a} :
  forall (l1:Init.Datatypes.list a) (l2:Init.Datatypes.list a)
    (l3:Init.Datatypes.list a),
  ((Init.Datatypes.app l1 (Init.Datatypes.app l2 l3)) =
   (Init.Datatypes.app (Init.Datatypes.app l1 l2) l3)).
Proof.
intros l1 l2 l3.
apply app_assoc.
Qed.

(* Why3 goal *)
Lemma Append_l_nil {a:Type} {a_WT:WhyType a} :
  forall (l:Init.Datatypes.list a),
  ((Init.Datatypes.app l Init.Datatypes.nil) = l).
Proof.
intros l.
apply app_nil_r.
Qed.

(* Why3 goal *)
Lemma Append_length {a:Type} {a_WT:WhyType a} :
  forall (l1:Init.Datatypes.list a) (l2:Init.Datatypes.list a),
  ((list.Length.length (Init.Datatypes.app l1 l2)) =
   ((list.Length.length l1) + (list.Length.length l2))%Z).
Proof.
intros l1 l2.
rewrite 3!Length.length_std.
now rewrite app_length, inj_plus.
Qed.

(* Why3 goal *)
Lemma mem_append {a:Type} {a_WT:WhyType a} :
  forall (x:a) (l1:Init.Datatypes.list a) (l2:Init.Datatypes.list a),
  list.Mem.mem x (Init.Datatypes.app l1 l2) <->
  list.Mem.mem x l1 \/ list.Mem.mem x l2.
Proof.
intros x l1 l2.
split.
intros H.
apply Mem.mem_std in H.
apply in_app_or in H.
destruct H as [H|H].
left.
now apply Mem.mem_std.
right.
now apply Mem.mem_std.
intros H.
apply Mem.mem_std.
apply in_or_app.
destruct H as [H|H].
left.
now apply Mem.mem_std.
right.
now apply Mem.mem_std.
Qed.

(* Why3 goal *)
Lemma mem_decomp {a:Type} {a_WT:WhyType a} :
  forall (x:a) (l:Init.Datatypes.list a), list.Mem.mem x l ->
  exists l1:Init.Datatypes.list a, exists l2:Init.Datatypes.list a,
  (l = (Init.Datatypes.app l1 (Init.Datatypes.cons x l2))).
Proof.
intros x l h1.
apply in_split.
now apply Mem.mem_std.
Qed.

