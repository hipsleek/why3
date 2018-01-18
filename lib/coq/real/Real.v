(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2018   --   Inria - CNRS - Paris-Sud University  *)
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

(* Why3 comment *)
(* prefix_mn is replaced with (-x)%R by the coq driver *)

(* Why3 comment *)
(* infix_pl is replaced with (x + x1)%R by the coq driver *)

(* Why3 comment *)
(* infix_as is replaced with (x * x1)%R by the coq driver *)

(* Why3 comment *)
(* infix_ls is replaced with (x < x1)%R by the coq driver *)

(* Why3 goal *)
Lemma infix_lseq_def : forall (x:R) (y:R), (x <= y)%R <-> ((x < y)%R \/
  (x = y)).
Proof.
reflexivity.
Qed.

(* Why3 goal *)
Lemma Assoc : forall (x:R) (y:R) (z:R),
  (((x + y)%R + z)%R = (x + (y + z)%R)%R).
Proof.
exact Rplus_assoc.
Qed.

(* Why3 goal *)
Lemma Unit_def_l : forall (x:R), ((0%R + x)%R = x).
Proof.
exact Rplus_0_l.
Qed.

(* Why3 goal *)
Lemma Unit_def_r : forall (x:R), ((x + 0%R)%R = x).
Proof.
exact Rplus_0_r.
Qed.

(* Why3 goal *)
Lemma Inv_def_l : forall (x:R), (((-x)%R + x)%R = 0%R).
Proof.
exact Rplus_opp_l.
Qed.

(* Why3 goal *)
Lemma Inv_def_r : forall (x:R), ((x + (-x)%R)%R = 0%R).
Proof.
exact Rplus_opp_r.
Qed.

(* Why3 goal *)
Lemma Comm : forall (x:R) (y:R), ((x + y)%R = (y + x)%R).
Proof.
exact Rplus_comm.
Qed.

(* Why3 goal *)
Lemma Assoc1 : forall (x:R) (y:R) (z:R),
  (((x * y)%R * z)%R = (x * (y * z)%R)%R).
Proof.
exact Rmult_assoc.
Qed.

(* Why3 goal *)
Lemma Mul_distr_l : forall (x:R) (y:R) (z:R),
  ((x * (y + z)%R)%R = ((x * y)%R + (x * z)%R)%R).
Proof.
intros x y z.
apply Rmult_plus_distr_l.
Qed.

(* Why3 goal *)
Lemma Mul_distr_r : forall (x:R) (y:R) (z:R),
  (((y + z)%R * x)%R = ((y * x)%R + (z * x)%R)%R).
Proof.
intros x y z.
apply Rmult_plus_distr_r.
Qed.

(* Why3 goal *)
Lemma Comm1 : forall (x:R) (y:R), ((x * y)%R = (y * x)%R).
Proof.
exact Rmult_comm.
Qed.

(* Why3 goal *)
Lemma Unitary : forall (x:R), ((1%R * x)%R = x).
Proof.
exact Rmult_1_l.
Qed.

(* Why3 goal *)
Lemma NonTrivialRing : ~ (0%R = 1%R).
Proof.
apply not_eq_sym.
exact R1_neq_R0.
Qed.

(* Why3 comment *)
(* inv is replaced with (Reals.Rdefinitions.Rinv x) by the coq driver *)

(* Why3 goal *)
Lemma Inverse : forall (x:R), (~ (x = 0%R)) ->
  ((x * (Reals.Rdefinitions.Rinv x))%R = 1%R).
Proof.
exact Rinv_r.
Qed.

(* Why3 goal *)
Lemma infix_mn_def : forall (x:R) (y:R), ((x - y)%R = (x + (-y)%R)%R).
Proof.
reflexivity.
Qed.

(* Why3 goal *)
Lemma infix_sl_def : forall (x:R) (y:R),
  ((x / y)%R = (x * (Reals.Rdefinitions.Rinv y))%R).
Proof.
reflexivity.
Qed.

(* Why3 goal *)
Lemma add_div : forall (x:R) (y:R) (z:R), (~ (z = 0%R)) ->
  (((x + y)%R / z)%R = ((x / z)%R + (y / z)%R)%R).
Proof.
intros.
field.
assumption.
Qed.

(* Why3 goal *)
Lemma sub_div : forall (x:R) (y:R) (z:R), (~ (z = 0%R)) ->
  (((x - y)%R / z)%R = ((x / z)%R - (y / z)%R)%R).
Proof.
intros.
field.
assumption.
Qed.

(* Why3 goal *)
Lemma neg_div : forall (x:R) (y:R), (~ (y = 0%R)) ->
  (((-x)%R / y)%R = (-(x / y)%R)%R).
Proof.
intros.
field.
assumption.
Qed.

(* Why3 goal *)
Lemma assoc_mul_div : forall (x:R) (y:R) (z:R), (~ (z = 0%R)) ->
  (((x * y)%R / z)%R = (x * (y / z)%R)%R).
Proof.
intros x y z _.
apply Rmult_assoc.
Qed.

(* Why3 goal *)
Lemma assoc_div_mul : forall (x:R) (y:R) (z:R), ((~ (y = 0%R)) /\
  ~ (z = 0%R)) -> (((x / y)%R / z)%R = (x / (y * z)%R)%R).
Proof.
intros x y z (Zy, Zz).
unfold Rdiv.
rewrite Rmult_assoc.
now rewrite Rinv_mult_distr.
Qed.

(* Why3 goal *)
Lemma assoc_div_div : forall (x:R) (y:R) (z:R), ((~ (y = 0%R)) /\
  ~ (z = 0%R)) -> ((x / (y / z)%R)%R = ((x * z)%R / y)%R).
Proof.
intros x y z (Zy, Zz).
field.
now split.
Qed.

(* Why3 goal *)
Lemma Refl : forall (x:R), (x <= x)%R.
Proof.
exact Rle_refl.
Qed.

(* Why3 goal *)
Lemma Trans : forall (x:R) (y:R) (z:R), (x <= y)%R -> ((y <= z)%R ->
  (x <= z)%R).
Proof.
exact Rle_trans.
Qed.

(* Why3 goal *)
Lemma Antisymm : forall (x:R) (y:R), (x <= y)%R -> ((y <= x)%R -> (x = y)).
Proof.
exact Rle_antisym.
Qed.

(* Why3 goal *)
Lemma Total : forall (x:R) (y:R), (x <= y)%R \/ (y <= x)%R.
Proof.
intros x y.
destruct (Rle_or_lt x y) as [H|H].
now left.
right.
now apply Rlt_le.
Qed.

(* Why3 goal *)
Lemma ZeroLessOne : (0%R <= 1%R)%R.
Proof.
exact Rle_0_1.
Qed.

(* Why3 goal *)
Lemma CompatOrderAdd : forall (x:R) (y:R) (z:R), (x <= y)%R ->
  ((x + z)%R <= (y + z)%R)%R.
Proof.
intros x y z.
exact (Rplus_le_compat_r z x y).
Qed.

(* Why3 goal *)
Lemma CompatOrderMult : forall (x:R) (y:R) (z:R), (x <= y)%R ->
  ((0%R <= z)%R -> ((x * z)%R <= (y * z)%R)%R).
Proof.
intros x y z H Zz.
now apply Rmult_le_compat_r.
Qed.

