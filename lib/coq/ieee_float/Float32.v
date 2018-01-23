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
Require Reals.Rbasic_fun.
Require Reals.R_sqrt.
Require BuiltIn.
Require int.Int.
Require real.Real.
Require real.RealInfix.
Require real.Abs.
Require real.FromInt.
Require real.Truncate.
Require real.Square.
Require bv.Pow2int.
Require ieee_float.RoundingMode.
Require ieee_float.GenericFloat.

Import Flocq.Core.Fcore.
Import Flocq.Appli.Fappli_IEEE.
Import ieee_float.RoundingMode.
Import ieee_float.GenericFloat.

(* Why3 goal *)
Definition t : Type.
Proof.
  exact (t 8 24).
Defined.

(* Why3 goal *)
Definition t'real : t -> R.
Proof.
  apply B2R.
Defined.

(* Why3 goal *)
Definition t'isFinite : t -> Prop.
Proof.
  apply is_finite.
Defined.

(* Why3 goal *)
Lemma t'axiom : forall (x:t), (t'isFinite x) ->
  (((-(16777215 * 20282409603651670423947251286016)%R)%R <= (t'real x))%R /\
  ((t'real x) <= (16777215 * 20282409603651670423947251286016)%R)%R).
Proof.
intros x _.
apply Rabs_le_inv.
change (Rabs (B2R _ _ x) <= F2R (Float radix2 (Zpower radix2 24 - 1) (127 - 23)))%R.
destruct x as [s|s|s|s m e H] ;
  try (simpl ; rewrite Rabs_R0 ; now apply F2R_ge_0_compat).
simpl.
rewrite <- F2R_Zabs.
rewrite abs_cond_Zopp.
apply andb_prop in H.
destruct H as [H1 H2].
apply Zeq_bool_eq in H1.
apply Zle_bool_imp_le in H2.
rewrite Fcore_digits.Zpos_digits2_pos in H1.
apply Rmult_le_compat.
now apply (Z2R_le 0).
apply bpow_ge_0.
apply Z2R_le.
apply (Z.lt_le_pred (Zabs (Zpos m)) (Zpower radix2 24)).
apply Fcore_digits.Zpower_gt_Zdigits.
revert H1.
generalize (Fcore_digits.Zdigits radix2 (Z.pos m)).
unfold FLT_exp, sb.
intros ; zify ; omega.
now apply bpow_le.
Qed.

(* Why3 goal *)
Definition zeroF : t.
Proof.
  apply zeroF.
Defined.

(* Why3 goal *)
Definition add : ieee_float.RoundingMode.mode -> t -> t -> t.
Proof.
  now apply add.
Defined.

(* Why3 goal *)
Definition sub : ieee_float.RoundingMode.mode -> t -> t -> t.
Proof.
  now apply sub.
Defined.

(* Why3 goal *)
Definition mul : ieee_float.RoundingMode.mode -> t -> t -> t.
Proof.
  now apply mul.
Defined.

(* Why3 goal *)
Definition div : ieee_float.RoundingMode.mode -> t -> t -> t.
Proof.
  now apply div.
Defined.

(* Why3 goal *)
Definition abs : t -> t.
Proof.
  apply abs.
Defined.

(* Why3 goal *)
Definition neg : t -> t.
Proof.
  apply neg.
Defined.

(* Why3 goal *)
Definition fma : ieee_float.RoundingMode.mode -> t -> t -> t -> t.
Proof.
  now apply fma.
Defined.

(* Why3 goal *)
Definition sqrt : ieee_float.RoundingMode.mode -> t -> t.
Proof.
  now apply GenericFloat.sqrt.
Defined.

(* Why3 goal *)
Definition roundToIntegral : ieee_float.RoundingMode.mode -> t -> t.
Proof.
  now apply roundToIntegral.
Defined.

(* Why3 goal *)
Definition min : t -> t -> t.
Proof.
  now apply min.
Defined.

(* Why3 goal *)
Definition max : t -> t -> t.
Proof.
  now apply max.
Defined.

(* Why3 goal *)
Definition le : t -> t -> Prop.
Proof.
  apply le.
Defined.

(* Why3 goal *)
Definition lt : t -> t -> Prop.
Proof.
  apply lt.
Defined.

(* Why3 goal *)
Definition eq : t -> t -> Prop.
Proof.
  apply eq.
Defined.

(* Why3 goal *)
Definition is_normal : t -> Prop.
Proof.
  apply is_normal.
Defined.

(* Why3 goal *)
Definition is_subnormal : t -> Prop.
Proof.
  apply is_subnormal.
Defined.

(* Why3 goal *)
Definition is_zero : t -> Prop.
Proof.
  apply is_zero.
Defined.

(* Why3 goal *)
Definition is_infinite : t -> Prop.
Proof.
  apply is_infinite.
Defined.

(* Why3 goal *)
Definition is_nan : t -> Prop.
Proof.
  apply is_nan.
Defined.

(* Why3 goal *)
Definition is_positive : t -> Prop.
Proof.
  apply is_positive.
Defined.

(* Why3 goal *)
Definition is_negative : t -> Prop.
Proof.
  apply is_negative.
Defined.

(* Why3 assumption *)
Definition is_plus_infinity (x:t) : Prop :=
  (is_infinite x) /\ (is_positive x).

(* Why3 assumption *)
Definition is_minus_infinity (x:t) : Prop :=
  (is_infinite x) /\ (is_negative x).

(* Why3 assumption *)
Definition is_plus_zero (x:t) : Prop := (is_zero x) /\ (is_positive x).

(* Why3 assumption *)
Definition is_minus_zero (x:t) : Prop := (is_zero x) /\ (is_negative x).

(* Why3 assumption *)
Definition is_not_nan (x:t) : Prop := (t'isFinite x) \/ (is_infinite x).

(* Why3 goal *)
Lemma is_not_nan1 : forall (x:t), (is_not_nan x) <-> ~ (is_nan x).
Proof.
  apply is_not_nan1.
Qed.

(* Why3 goal *)
Lemma is_not_finite :
  forall (x:t), (~ (t'isFinite x)) <-> ((is_infinite x) \/ (is_nan x)).
Proof.
  apply is_not_finite.
Qed.

(* Why3 goal *)
Lemma zeroF_is_positive : is_positive zeroF.
Proof.
  apply zeroF_is_positive.
Qed.

(* Why3 goal *)
Lemma zeroF_is_zero : is_zero zeroF.
Proof.
  apply zeroF_is_zero.
Qed.

(* Why3 goal *)
Lemma zero_to_real :
  forall (x:t), (is_zero x) <-> ((t'isFinite x) /\ ((t'real x) = 0%R)).
Proof.
  apply zero_to_real.
Qed.

(* Why3 goal *)
Definition of_int : ieee_float.RoundingMode.mode -> Z -> t.
Proof.
  now apply z_to_fp.
Defined.

(* Why3 goal *)
Definition to_int : ieee_float.RoundingMode.mode -> t -> Z.
Proof.
  now apply fp_to_z.
Defined.

(* Why3 goal *)
Lemma zero_of_int :
  forall (m:ieee_float.RoundingMode.mode), (zeroF = (of_int m 0%Z)).
Proof.
  apply zero_of_int.
Qed.

(* Why3 goal *)
Definition round : ieee_float.RoundingMode.mode -> R -> R.
Proof.
  apply (round 8 24).
Defined.

Lemma max_real_cst :
  max_real 8 24 = (33554430 * 10141204801825835211973625643008)%R.
Proof.
  change (33554430 * 10141204801825835211973625643008)%R
    with (F2R (Float radix2 (16777215 * Zpower radix2 (104 - 103)) 103)).
  rewrite <- F2R_change_exp by easy.
  now rewrite <- max_real_is_F2R.
Qed.

(* Why3 goal *)
Definition max_int : Z.
Proof.
  exact (33554430 * 10141204801825835211973625643008)%Z.
Defined.

(* Why3 goal *)
Lemma max_real_int :
  ((33554430 * 10141204801825835211973625643008)%R = (BuiltIn.IZR max_int)).
Proof.
  unfold max_int.
  now rewrite mult_IZR, <- !Z2R_IZR.
Qed.

(* Why3 assumption *)
Definition in_range (x:R) : Prop :=
  ((-(33554430 * 10141204801825835211973625643008)%R)%R <= x)%R /\
  (x <= (33554430 * 10141204801825835211973625643008)%R)%R.

(* Why3 assumption *)
Definition in_int_range (i:Z) : Prop :=
  ((-max_int)%Z <= i)%Z /\ (i <= max_int)%Z.

(* Why3 goal *)
Lemma is_finite : forall (x:t), (t'isFinite x) -> in_range (t'real x).
Proof.
  unfold t'isFinite, in_range.
  intros x Hx.
  rewrite <- max_real_cst.
  now apply is_finite1.
Qed.

(* Why3 assumption *)
Definition no_overflow (m:ieee_float.RoundingMode.mode) (x:R) : Prop :=
  in_range (round m x).

(* Why3 goal *)
Lemma Bounded_real_no_overflow :
  forall (m:ieee_float.RoundingMode.mode) (x:R),
  (in_range x) -> no_overflow m x.
Proof.
  unfold no_overflow, in_range.
  rewrite <- max_real_cst.
  now apply (Bounded_real_no_overflow 8 24).
Qed.

(* Why3 goal *)
Lemma Round_monotonic :
  forall (m:ieee_float.RoundingMode.mode) (x:R) (y:R),
  (x <= y)%R -> ((round m x) <= (round m y))%R.
Proof.
  apply Round_monotonic.
Qed.

(* Why3 goal *)
Lemma Round_idempotent : forall (m1:ieee_float.RoundingMode.mode)
  (m2:ieee_float.RoundingMode.mode) (x:R), ((round m1 (round m2
  x)) = (round m2 x)).
Proof.
  apply Round_idempotent.
Qed.

(* Why3 goal *)
Lemma Round_to_real :
  forall (m:ieee_float.RoundingMode.mode) (x:t),
  (t'isFinite x) -> ((round m (t'real x)) = (t'real x)).
Proof.
  apply Round_to_real.
Qed.

(* Why3 goal *)
Lemma Round_down_le :
  forall (x:R), ((round ieee_float.RoundingMode.RTN x) <= x)%R.
Proof.
  apply Round_down_le.
Qed.

(* Why3 goal *)
Lemma Round_up_ge :
  forall (x:R), (x <= (round ieee_float.RoundingMode.RTP x))%R.
Proof.
  apply Round_up_ge.
Qed.

(* Why3 goal *)
Lemma Round_down_neg :
  forall (x:R),
  ((round ieee_float.RoundingMode.RTN (-x)%R) =
   (-(round ieee_float.RoundingMode.RTP x))%R).
Proof.
  apply Round_down_neg.
Qed.

(* Why3 goal *)
Lemma Round_up_neg :
  forall (x:R),
  ((round ieee_float.RoundingMode.RTP (-x)%R) =
   (-(round ieee_float.RoundingMode.RTN x))%R).
Proof.
  apply Round_up_neg.
Qed.

(* Why3 assumption *)
Definition in_safe_int_range (i:Z) : Prop :=
  ((-16777216%Z)%Z <= i)%Z /\ (i <= 16777216%Z)%Z.

(* Why3 goal *)
Lemma Exact_rounding_for_integers :
  forall (m:ieee_float.RoundingMode.mode) (i:Z),
  (in_safe_int_range i) -> ((round m (BuiltIn.IZR i)) = (BuiltIn.IZR i)).
Proof.
  intros m i h1.
  now apply Exact_rounding_for_integers.
Qed.

(* Why3 assumption *)
Definition same_sign (x:t) (y:t) : Prop :=
  ((is_positive x) /\ (is_positive y)) \/
  ((is_negative x) /\ (is_negative y)).

(* Why3 assumption *)
Definition diff_sign (x:t) (y:t) : Prop :=
  ((is_positive x) /\ (is_negative y)) \/
  ((is_negative x) /\ (is_positive y)).

(* Why3 goal *)
Lemma feq_eq :
  forall (x:t) (y:t),
  (t'isFinite x) -> (t'isFinite y) -> ~ (is_zero x) -> (eq x y) -> (x = y).
Proof.
  apply feq_eq.
Qed.

(* Why3 goal *)
Lemma eq_feq :
  forall (x:t) (y:t), (t'isFinite x) -> (t'isFinite y) -> (x = y) -> eq x y.
Proof.
  apply eq_feq.
Qed.

(* Why3 goal *)
Lemma eq_refl : forall (x:t), (t'isFinite x) -> eq x x.
Proof.
  apply eq_refl.
Qed.

(* Why3 goal *)
Lemma eq_sym : forall (x:t) (y:t), (eq x y) -> eq y x.
Proof.
  apply eq_sym.
Qed.

(* Why3 goal *)
Lemma eq_trans : forall (x:t) (y:t) (z:t), (eq x y) -> (eq y z) -> eq x z.
Proof.
  apply eq_trans.
Qed.

(* Why3 goal *)
Lemma eq_zero : eq zeroF (neg zeroF).
Proof.
  apply eq_zero.
Qed.

(* Why3 goal *)
Lemma eq_to_real_finite :
  forall (x:t) (y:t),
  ((t'isFinite x) /\ (t'isFinite y)) ->
  (eq x y) <-> ((t'real x) = (t'real y)).
Proof.
  apply eq_to_real_finite.
Qed.

(* Why3 goal *)
Lemma eq_special : forall (x:t) (y:t), (eq x y) -> ((is_not_nan x) /\
  ((is_not_nan y) /\ (((t'isFinite x) /\ (t'isFinite y)) \/ ((is_infinite
  x) /\ ((is_infinite y) /\ (same_sign x y)))))).
Proof.
  apply eq_special.
Qed.

(* Why3 goal *)
Lemma lt_finite :
  forall (x:t) (y:t),
  ((t'isFinite x) /\ (t'isFinite y)) ->
  (lt x y) <-> ((t'real x) < (t'real y))%R.
Proof.
  apply lt_finite.
Qed.

(* Why3 goal *)
Lemma le_finite :
  forall (x:t) (y:t),
  ((t'isFinite x) /\ (t'isFinite y)) ->
  (le x y) <-> ((t'real x) <= (t'real y))%R.
Proof.
  apply le_finite.
Qed.

(* Why3 goal *)
Lemma le_lt_trans :
  forall (x:t) (y:t) (z:t), ((le x y) /\ (lt y z)) -> lt x z.
Proof.
  apply le_lt_trans.
Qed.

(* Why3 goal *)
Lemma lt_le_trans :
  forall (x:t) (y:t) (z:t), ((lt x y) /\ (le y z)) -> lt x z.
Proof.
  apply lt_le_trans.
Qed.

(* Why3 goal *)
Lemma le_ge_asym : forall (x:t) (y:t), ((le x y) /\ (le y x)) -> eq x y.
Proof.
  apply le_ge_asym.
Qed.

(* Why3 goal *)
Lemma not_lt_ge :
  forall (x:t) (y:t),
  (~ (lt x y) /\ ((is_not_nan x) /\ (is_not_nan y))) -> le y x.
Proof.
  apply not_lt_ge.
Qed.

(* Why3 goal *)
Lemma not_gt_le :
  forall (x:t) (y:t),
  (~ (lt y x) /\ ((is_not_nan x) /\ (is_not_nan y))) -> le x y.
Proof.
 apply not_gt_le.
Qed.

(* Why3 goal *)
Lemma le_special : forall (x:t) (y:t), (le x y) -> (((t'isFinite x) /\
  (t'isFinite y)) \/ (((is_minus_infinity x) /\ (is_not_nan y)) \/
  ((is_not_nan x) /\ (is_plus_infinity y)))).
Proof.
  apply le_special.
Qed.

(* Why3 goal *)
Lemma lt_special : forall (x:t) (y:t), (lt x y) -> (((t'isFinite x) /\
  (t'isFinite y)) \/ (((is_minus_infinity x) /\ ((is_not_nan y) /\
  ~ (is_minus_infinity y))) \/ ((is_not_nan x) /\ ((~ (is_plus_infinity
  x)) /\ (is_plus_infinity y))))).
Proof.
  apply lt_special.
Qed.

(* Why3 goal *)
Lemma lt_lt_finite :
  forall (x:t) (y:t) (z:t), (lt x y) -> (lt y z) -> t'isFinite y.
Proof.
  apply lt_lt_finite.
Qed.

(* Why3 goal *)
Lemma positive_to_real :
  forall (x:t), (t'isFinite x) -> (is_positive x) -> (0%R <= (t'real x))%R.
Proof.
  apply positive_to_real.
Qed.

(* Why3 goal *)
Lemma to_real_positive :
  forall (x:t), (t'isFinite x) -> (0%R < (t'real x))%R -> is_positive x.
Proof.
  apply to_real_positive.
Qed.

(* Why3 goal *)
Lemma negative_to_real :
  forall (x:t), (t'isFinite x) -> (is_negative x) -> ((t'real x) <= 0%R)%R.
Proof.
  apply negative_to_real.
Qed.

(* Why3 goal *)
Lemma to_real_negative :
  forall (x:t), (t'isFinite x) -> ((t'real x) < 0%R)%R -> is_negative x.
Proof.
  apply to_real_negative.
Qed.

(* Why3 goal *)
Lemma negative_xor_positive :
  forall (x:t), ~ ((is_positive x) /\ (is_negative x)).
Proof.
  apply negative_xor_positive.
Qed.

(* Why3 goal *)
Lemma negative_or_positive :
  forall (x:t), (is_not_nan x) -> (is_positive x) \/ (is_negative x).
Proof.
  apply negative_or_positive.
Qed.

(* Why3 goal *)
Lemma diff_sign_trans :
  forall (x:t) (y:t) (z:t),
  ((diff_sign x y) /\ (diff_sign y z)) -> same_sign x z.
Proof.
  apply diff_sign_trans.
Qed.

(* Why3 goal *)
Lemma diff_sign_product : forall (x:t) (y:t), ((t'isFinite x) /\ ((t'isFinite
  y) /\ (((t'real x) * (t'real y))%R < 0%R)%R)) -> (diff_sign x y).
Proof.
  apply diff_sign_product.
Qed.

(* Why3 goal *)
Lemma same_sign_product :
  forall (x:t) (y:t),
  ((t'isFinite x) /\ ((t'isFinite y) /\ (same_sign x y))) ->
  (0%R <= ((t'real x) * (t'real y))%R)%R.
Proof.
  apply same_sign_product.
Qed.

(* Why3 assumption *)
Definition product_sign (z:t) (x:t) (y:t) : Prop :=
  ((same_sign x y) -> is_positive z) /\ ((diff_sign x y) -> is_negative z).

(* Why3 assumption *)
Definition overflow_value (m:ieee_float.RoundingMode.mode) (x:t) : Prop :=
  match m with
  | ieee_float.RoundingMode.RTN => ((is_positive x) -> ((t'isFinite x) /\
      ((t'real x) = (33554430 * 10141204801825835211973625643008)%R))) /\
      ((~ (is_positive x)) -> (is_infinite x))
  | ieee_float.RoundingMode.RTP => ((is_positive x) -> (is_infinite x)) /\
      ((~ (is_positive x)) -> ((t'isFinite x) /\
      ((t'real x) = (-(33554430 * 10141204801825835211973625643008)%R)%R)))
  | ieee_float.RoundingMode.RTZ => ((is_positive x) -> ((t'isFinite x) /\
      ((t'real x) = (33554430 * 10141204801825835211973625643008)%R))) /\
      ((~ (is_positive x)) -> ((t'isFinite x) /\
      ((t'real x) = (-(33554430 * 10141204801825835211973625643008)%R)%R)))
  | (ieee_float.RoundingMode.RNA|ieee_float.RoundingMode.RNE) => (is_infinite
      x)
  end.

(* Why3 assumption *)
Definition sign_zero_result (m:ieee_float.RoundingMode.mode) (x:t) : Prop :=
  (is_zero x) ->
  match m with
  | ieee_float.RoundingMode.RTN => is_negative x
  | _ => is_positive x
  end.

(* Why3 goal *)
Lemma add_finite : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  (t'isFinite x) -> ((t'isFinite y) -> ((no_overflow m
  ((t'real x) + (t'real y))%R) -> ((t'isFinite (add m x y)) /\
  ((t'real (add m x y)) = (round m ((t'real x) + (t'real y))%R))))).
Proof.
  intros m x y h1 h2 h3.
  apply add_finite ; try easy.
  unfold no_overflow, in_range in h3.
  now rewrite <- max_real_cst in h3.
Qed.

(* Why3 goal *)
Lemma add_finite_rev :
  forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  (t'isFinite (add m x y)) -> ((t'isFinite x) /\ (t'isFinite y)).
Proof.
  apply add_finite_rev.
Qed.

(* Why3 goal *)
Lemma add_finite_rev_n : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  (ieee_float.RoundingMode.to_nearest m) -> ((t'isFinite (add m x y)) ->
  ((no_overflow m ((t'real x) + (t'real y))%R) /\ ((t'real (add m x
  y)) = (round m ((t'real x) + (t'real y))%R)))).
Proof.
  intros m x y h1 h2.
  unfold no_overflow, in_range.
  rewrite <- max_real_cst.
  now apply add_finite_rev_n.
Qed.

(* Why3 goal *)
Lemma sub_finite : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  (t'isFinite x) -> ((t'isFinite y) -> ((no_overflow m
  ((t'real x) - (t'real y))%R) -> ((t'isFinite (sub m x y)) /\
  ((t'real (sub m x y)) = (round m ((t'real x) - (t'real y))%R))))).
Proof.
  intros m x y h1 h2 h3.
  apply sub_finite ; try easy.
  unfold no_overflow, in_range in h3.
  now rewrite <- max_real_cst in h3.
Qed.

(* Why3 goal *)
Lemma sub_finite_rev :
  forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  (t'isFinite (sub m x y)) -> (t'isFinite x) /\ (t'isFinite y).
Proof.
  apply sub_finite_rev.
Qed.

(* Why3 goal *)
Lemma sub_finite_rev_n : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  (ieee_float.RoundingMode.to_nearest m) -> ((t'isFinite (sub m x y)) ->
  ((no_overflow m ((t'real x) - (t'real y))%R) /\ ((t'real (sub m x
  y)) = (round m ((t'real x) - (t'real y))%R)))).
Proof.
  intros m x y h1 h2.
  unfold no_overflow, in_range.
  rewrite <- max_real_cst.
  now apply sub_finite_rev_n.
Qed.

(* Why3 goal *)
Lemma mul_finite : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  (t'isFinite x) -> ((t'isFinite y) -> ((no_overflow m
  ((t'real x) * (t'real y))%R) -> ((t'isFinite (mul m x y)) /\
  ((t'real (mul m x y)) = (round m ((t'real x) * (t'real y))%R))))).
Proof.
  intros m x y h1 h2 h3.
  apply mul_finite ; try easy.
  unfold no_overflow, in_range in h3.
  now rewrite <- max_real_cst in h3.
Qed.

(* Why3 goal *)
Lemma mul_finite_rev :
  forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  (t'isFinite (mul m x y)) -> (t'isFinite x) /\ (t'isFinite y).
Proof.
  apply mul_finite_rev.
Qed.

(* Why3 goal *)
Lemma mul_finite_rev_n : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  (ieee_float.RoundingMode.to_nearest m) -> ((t'isFinite (mul m x y)) ->
  ((no_overflow m ((t'real x) * (t'real y))%R) /\ ((t'real (mul m x
  y)) = (round m ((t'real x) * (t'real y))%R)))).
Proof.
  intros m x y h1 h2.
  unfold no_overflow, in_range.
  rewrite <- max_real_cst.
  now apply mul_finite_rev_n.
Qed.

(* Why3 goal *)
Lemma div_finite : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  (t'isFinite x) -> ((t'isFinite y) -> ((~ (is_zero y)) -> ((no_overflow m
  ((t'real x) / (t'real y))%R) -> ((t'isFinite (div m x y)) /\
  ((t'real (div m x y)) = (round m ((t'real x) / (t'real y))%R)))))).
Proof.
  intros m x y h1 h2 h3 h4.
  apply div_finite ; try easy.
  unfold no_overflow, in_range in h4.
  now rewrite <- max_real_cst in h4.
Qed.

(* Why3 goal *)
Lemma div_finite_rev :
  forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  (t'isFinite (div m x y)) ->
  ((t'isFinite x) /\ ((t'isFinite y) /\ ~ (is_zero y))) \/
  ((t'isFinite x) /\ ((is_infinite y) /\ ((t'real (div m x y)) = 0%R))).
Proof.
  apply div_finite_rev.
Qed.

(* Why3 goal *)
Lemma div_finite_rev_n : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  (ieee_float.RoundingMode.to_nearest m) -> ((t'isFinite (div m x y)) ->
  ((t'isFinite y) -> ((no_overflow m ((t'real x) / (t'real y))%R) /\
  ((t'real (div m x y)) = (round m ((t'real x) / (t'real y))%R))))).
Proof.
  intros m x y h1 h2 h3.
  unfold no_overflow, in_range.
  rewrite <- max_real_cst.
  now apply div_finite_rev_n.
Qed.

(* Why3 goal *)
Lemma neg_finite :
  forall (x:t),
  (t'isFinite x) ->
  (t'isFinite (neg x)) /\ ((t'real (neg x)) = (-(t'real x))%R).
Proof.
  apply neg_finite.
Qed.

(* Why3 goal *)
Lemma neg_finite_rev :
  forall (x:t),
  (t'isFinite (neg x)) ->
  (t'isFinite x) /\ ((t'real (neg x)) = (-(t'real x))%R).
Proof.
  apply neg_finite_rev.
Qed.

(* Why3 goal *)
Lemma abs_finite : forall (x:t), (t'isFinite x) -> ((t'isFinite (abs x)) /\
  (((t'real (abs x)) = (Reals.Rbasic_fun.Rabs (t'real x))) /\ (is_positive
  (abs x)))).
Proof.
  apply abs_finite.
Qed.

(* Why3 goal *)
Lemma abs_finite_rev :
  forall (x:t),
  (t'isFinite (abs x)) ->
  (t'isFinite x) /\ ((t'real (abs x)) = (Reals.Rbasic_fun.Rabs (t'real x))).
Proof.
  apply abs_finite_rev.
Qed.

(* Why3 goal *)
Lemma abs_universal : forall (x:t), ~ (is_negative (abs x)).
Proof.
  apply abs_universal.
Qed.

(* Why3 goal *)
Lemma fma_finite : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t) (z:t),
  (t'isFinite x) -> ((t'isFinite y) -> ((t'isFinite z) -> ((no_overflow m
  (((t'real x) * (t'real y))%R + (t'real z))%R) -> ((t'isFinite (fma m x y
  z)) /\ ((t'real (fma m x y z)) = (round m
  (((t'real x) * (t'real y))%R + (t'real z))%R)))))).
Proof.
  intros m x y z h1 h2 h3 h4.
  apply fma_finite ; try easy.
  unfold no_overflow, in_range in h4.
  now rewrite <- max_real_cst in h4.
Qed.

(* Why3 goal *)
Lemma fma_finite_rev :
  forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t) (z:t),
  (t'isFinite (fma m x y z)) ->
  ((t'isFinite x) /\ ((t'isFinite y) /\ (t'isFinite z))).
Proof.
  apply fma_finite_rev.
Qed.

(* Why3 goal *)
Lemma fma_finite_rev_n : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t)
  (z:t), (ieee_float.RoundingMode.to_nearest m) -> ((t'isFinite (fma m x y
  z)) -> ((no_overflow m (((t'real x) * (t'real y))%R + (t'real z))%R) /\
  ((t'real (fma m x y z)) = (round m
  (((t'real x) * (t'real y))%R + (t'real z))%R)))).
Proof.
  intros m x y z h1 h2.
  unfold no_overflow, in_range.
  rewrite <- max_real_cst.
  now apply fma_finite_rev_n.
Qed.

(* Why3 goal *)
Lemma sqrt_finite : forall (m:ieee_float.RoundingMode.mode) (x:t),
  (t'isFinite x) -> ((0%R <= (t'real x))%R -> ((t'isFinite (sqrt m x)) /\
  ((t'real (sqrt m x)) = (round m (Reals.R_sqrt.sqrt (t'real x)))))).
Proof.
  apply sqrt_finite.
Qed.

(* Why3 goal *)
Lemma sqrt_finite_rev : forall (m:ieee_float.RoundingMode.mode) (x:t),
  (t'isFinite (sqrt m x)) -> ((t'isFinite x) /\ ((0%R <= (t'real x))%R /\
  ((t'real (sqrt m x)) = (round m (Reals.R_sqrt.sqrt (t'real x)))))).
Proof.
  apply sqrt_finite_rev.
Qed.

(* Why3 assumption *)
Definition same_sign_real (x:t) (r:R) : Prop :=
  ((is_positive x) /\ (0%R < r)%R) \/ ((is_negative x) /\ (r < 0%R)%R).

(* Why3 goal *)
Lemma add_special : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  let r := (add m x y) in ((((is_nan x) \/ (is_nan y)) -> (is_nan r)) /\
  ((((t'isFinite x) /\ (is_infinite y)) -> ((is_infinite r) /\ (same_sign r
  y))) /\ ((((is_infinite x) /\ (t'isFinite y)) -> ((is_infinite r) /\
  (same_sign r x))) /\ ((((is_infinite x) /\ ((is_infinite y) /\ (same_sign x
  y))) -> ((is_infinite r) /\ (same_sign r x))) /\ ((((is_infinite x) /\
  ((is_infinite y) /\ (diff_sign x y))) -> (is_nan r)) /\ ((((t'isFinite
  x) /\ ((t'isFinite y) /\ ~ (no_overflow m ((t'real x) + (t'real y))%R))) ->
  ((same_sign_real r ((t'real x) + (t'real y))%R) /\ (overflow_value m
  r))) /\ (((t'isFinite x) /\ (t'isFinite y)) -> (((same_sign x y) ->
  (same_sign r x)) /\ ((~ (same_sign x y)) -> (sign_zero_result m
  r)))))))))).
Proof.
  intros m x y r.
  unfold no_overflow, in_range, overflow_value.
  rewrite <- max_real_cst.
  apply add_special.
Qed.

(* Why3 goal *)
Lemma sub_special : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  let r := (sub m x y) in ((((is_nan x) \/ (is_nan y)) -> (is_nan r)) /\
  ((((t'isFinite x) /\ (is_infinite y)) -> ((is_infinite r) /\ (diff_sign r
  y))) /\ ((((is_infinite x) /\ (t'isFinite y)) -> ((is_infinite r) /\
  (same_sign r x))) /\ ((((is_infinite x) /\ ((is_infinite y) /\ (same_sign x
  y))) -> (is_nan r)) /\ ((((is_infinite x) /\ ((is_infinite y) /\ (diff_sign
  x y))) -> ((is_infinite r) /\ (same_sign r x))) /\ ((((t'isFinite x) /\
  ((t'isFinite y) /\ ~ (no_overflow m ((t'real x) - (t'real y))%R))) ->
  ((same_sign_real r ((t'real x) - (t'real y))%R) /\ (overflow_value m
  r))) /\ (((t'isFinite x) /\ (t'isFinite y)) -> (((diff_sign x y) ->
  (same_sign r x)) /\ ((~ (diff_sign x y)) -> (sign_zero_result m
  r)))))))))).
Proof.
  intros m x y r.
  unfold no_overflow, in_range, overflow_value.
  rewrite <- max_real_cst.
  apply sub_special.
Qed.

(* Why3 goal *)
Lemma mul_special : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  let r := (mul m x y) in ((((is_nan x) \/ (is_nan y)) -> (is_nan r)) /\
  ((((is_zero x) /\ (is_infinite y)) -> (is_nan r)) /\ ((((t'isFinite x) /\
  ((is_infinite y) /\ ~ (is_zero x))) -> (is_infinite r)) /\ ((((is_infinite
  x) /\ (is_zero y)) -> (is_nan r)) /\ ((((is_infinite x) /\ ((t'isFinite
  y) /\ ~ (is_zero y))) -> (is_infinite r)) /\ ((((is_infinite x) /\
  (is_infinite y)) -> (is_infinite r)) /\ ((((t'isFinite x) /\ ((t'isFinite
  y) /\ ~ (no_overflow m ((t'real x) * (t'real y))%R))) -> (overflow_value m
  r)) /\ ((~ (is_nan r)) -> (product_sign r x y))))))))).
Proof.
  intros m x y r.
  unfold no_overflow, in_range, overflow_value.
  rewrite <- max_real_cst.
  apply mul_special.
Qed.

(* Why3 goal *)
Lemma div_special : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  let r := (div m x y) in ((((is_nan x) \/ (is_nan y)) -> (is_nan r)) /\
  ((((t'isFinite x) /\ (is_infinite y)) -> (is_zero r)) /\ ((((is_infinite
  x) /\ (t'isFinite y)) -> (is_infinite r)) /\ ((((is_infinite x) /\
  (is_infinite y)) -> (is_nan r)) /\ ((((t'isFinite x) /\ ((t'isFinite y) /\
  ((~ (is_zero y)) /\ ~ (no_overflow m ((t'real x) / (t'real y))%R)))) ->
  (overflow_value m r)) /\ ((((t'isFinite x) /\ ((is_zero y) /\ ~ (is_zero
  x))) -> (is_infinite r)) /\ ((((is_zero x) /\ (is_zero y)) -> (is_nan
  r)) /\ ((~ (is_nan r)) -> (product_sign r x y))))))))).
Proof.
  intros m x y r.
  unfold no_overflow, in_range, overflow_value.
  rewrite <- max_real_cst.
  apply div_special.
Qed.

(* Why3 goal *)
Lemma neg_special :
  forall (x:t),
  ((is_nan x) -> is_nan (neg x)) /\
  (((is_infinite x) -> is_infinite (neg x)) /\
   (~ (is_nan x) -> diff_sign x (neg x))).
Proof.
  apply neg_special.
Qed.

(* Why3 goal *)
Lemma abs_special :
  forall (x:t),
  ((is_nan x) -> is_nan (abs x)) /\
  (((is_infinite x) -> is_infinite (abs x)) /\
   (~ (is_nan x) -> is_positive (abs x))).
Proof.
  apply abs_special.
Qed.

(* Why3 goal *)
Lemma fma_special : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t)
  (z:t), let r := (fma m x y z) in ((((is_nan x) \/ ((is_nan y) \/ (is_nan
  z))) -> (is_nan r)) /\ ((((is_zero x) /\ (is_infinite y)) -> (is_nan r)) /\
  ((((is_infinite x) /\ (is_zero y)) -> (is_nan r)) /\ ((((t'isFinite x) /\
  ((~ (is_zero x)) /\ ((is_infinite y) /\ (t'isFinite z)))) -> ((is_infinite
  r) /\ (product_sign r x y))) /\ ((((t'isFinite x) /\ ((~ (is_zero x)) /\
  ((is_infinite y) /\ (is_infinite z)))) -> (((product_sign z x y) ->
  ((is_infinite r) /\ (same_sign r z))) /\ ((~ (product_sign z x y)) ->
  (is_nan r)))) /\ ((((is_infinite x) /\ ((t'isFinite y) /\ ((~ (is_zero
  y)) /\ (t'isFinite z)))) -> ((is_infinite r) /\ (product_sign r x y))) /\
  ((((is_infinite x) /\ ((t'isFinite y) /\ ((~ (is_zero y)) /\ (is_infinite
  z)))) -> (((product_sign z x y) -> ((is_infinite r) /\ (same_sign r z))) /\
  ((~ (product_sign z x y)) -> (is_nan r)))) /\ ((((is_infinite x) /\
  ((is_infinite y) /\ (t'isFinite z))) -> ((is_infinite r) /\ (product_sign r
  x y))) /\ ((((t'isFinite x) /\ ((t'isFinite y) /\ (is_infinite z))) ->
  ((is_infinite r) /\ (same_sign r z))) /\ ((((is_infinite x) /\
  ((is_infinite y) /\ (is_infinite z))) -> (((product_sign z x y) ->
  ((is_infinite r) /\ (same_sign r z))) /\ ((~ (product_sign z x y)) ->
  (is_nan r)))) /\ ((((t'isFinite x) /\ ((t'isFinite y) /\ ((t'isFinite z) /\
  ~ (no_overflow m (((t'real x) * (t'real y))%R + (t'real z))%R)))) ->
  ((same_sign_real r (((t'real x) * (t'real y))%R + (t'real z))%R) /\
  (overflow_value m r))) /\ (((t'isFinite x) /\ ((t'isFinite y) /\
  (t'isFinite z))) -> (((product_sign z x y) -> (same_sign r z)) /\
  ((~ (product_sign z x y)) ->
  (((((t'real x) * (t'real y))%R + (t'real z))%R = 0%R) ->
  (((m = ieee_float.RoundingMode.RTN) -> (is_negative r)) /\
  ((~ (m = ieee_float.RoundingMode.RTN)) -> (is_positive r)))))))))))))))))).
Proof.
  intros m x y z r.
  unfold no_overflow, in_range, overflow_value.
  rewrite <- max_real_cst.
  apply fma_special.
Qed.

(* Why3 goal *)
Lemma sqrt_special : forall (m:ieee_float.RoundingMode.mode) (x:t), let r :=
  (sqrt m x) in (((is_nan x) -> (is_nan r)) /\ (((is_plus_infinity x) ->
  (is_plus_infinity r)) /\ (((is_minus_infinity x) -> (is_nan r)) /\
  ((((t'isFinite x) /\ ((t'real x) < 0%R)%R) -> (is_nan r)) /\ (((is_zero
  x) -> (same_sign r x)) /\ (((t'isFinite x) /\ (0%R < (t'real x))%R) ->
  (is_positive r))))))).
Proof.
  apply sqrt_special.
Qed.

(* Why3 goal *)
Lemma of_int_add_exact : forall (m:ieee_float.RoundingMode.mode)
  (n:ieee_float.RoundingMode.mode) (i:Z) (j:Z), (in_safe_int_range i) ->
  ((in_safe_int_range j) -> ((in_safe_int_range (i + j)%Z) -> (eq (of_int m
  (i + j)%Z) (add n (of_int m i) (of_int m j))))).
Proof.
  intros m n i j h1 h2 h3.
  now apply of_int_add_exact.
Qed.

(* Why3 goal *)
Lemma of_int_sub_exact : forall (m:ieee_float.RoundingMode.mode)
  (n:ieee_float.RoundingMode.mode) (i:Z) (j:Z), (in_safe_int_range i) ->
  ((in_safe_int_range j) -> ((in_safe_int_range (i - j)%Z) -> (eq (of_int m
  (i - j)%Z) (sub n (of_int m i) (of_int m j))))).
Proof.
  intros m n i j h1 h2 h3.
  now apply of_int_sub_exact.
Qed.

(* Why3 goal *)
Lemma of_int_mul_exact : forall (m:ieee_float.RoundingMode.mode)
  (n:ieee_float.RoundingMode.mode) (i:Z) (j:Z), (in_safe_int_range i) ->
  ((in_safe_int_range j) -> ((in_safe_int_range (i * j)%Z) -> (eq (of_int m
  (i * j)%Z) (mul n (of_int m i) (of_int m j))))).
Proof.
  intros m n i j h1 h2 h3.
  now apply of_int_mul_exact.
Qed.

(* Why3 goal *)
Lemma Min_r : forall (x:t) (y:t), (le y x) -> eq (min x y) y.
Proof.
  apply Min_r.
Qed.

(* Why3 goal *)
Lemma Min_l : forall (x:t) (y:t), (le x y) -> eq (min x y) x.
Proof.
  apply Min_l.
Qed.

(* Why3 goal *)
Lemma Max_r : forall (x:t) (y:t), (le y x) -> eq (max x y) x.
Proof.
  apply Max_r.
Qed.

(* Why3 goal *)
Lemma Max_l : forall (x:t) (y:t), (le x y) -> eq (max x y) y.
Proof.
  apply Max_l.
Qed.

(* Why3 goal *)
Definition is_int : t -> Prop.
Proof.
  apply is_int.
Defined.

(* Why3 goal *)
Lemma zeroF_is_int : is_int zeroF.
Proof.
  apply zeroF_is_int.
Qed.

(* Why3 goal *)
Lemma of_int_is_int :
  forall (m:ieee_float.RoundingMode.mode) (x:Z),
  (in_int_range x) -> is_int (of_int m x).
Proof.
  intros m x h1.
  now apply of_int_is_int.
Qed.

(* Why3 goal *)
Lemma big_float_is_int :
  forall (m:ieee_float.RoundingMode.mode) (i:t),
  (t'isFinite i) ->
  ((le i (neg (of_int m 16777216%Z))) \/ (le (of_int m 16777216%Z) i)) ->
  is_int i.
Proof.
  now apply big_float_is_int.
Qed.

(* Why3 goal *)
Lemma roundToIntegral_is_int :
  forall (m:ieee_float.RoundingMode.mode) (x:t),
  (t'isFinite x) -> is_int (roundToIntegral m x).
Proof.
  now apply roundToIntegral_is_int.
Qed.

(* Why3 goal *)
Lemma eq_is_int : forall (x:t) (y:t), (eq x y) -> (is_int x) -> is_int y.
Proof.
  apply eq_is_int.
Qed.

(* Why3 goal *)
Lemma add_int :
  forall (x:t) (y:t) (m:ieee_float.RoundingMode.mode),
  (is_int x) -> (is_int y) -> (t'isFinite (add m x y)) -> is_int (add m x y).
Proof.
  apply add_int.
Qed.

(* Why3 goal *)
Lemma sub_int :
  forall (x:t) (y:t) (m:ieee_float.RoundingMode.mode),
  (is_int x) -> (is_int y) -> (t'isFinite (sub m x y)) -> is_int (sub m x y).
Proof.
  apply sub_int.
Qed.

(* Why3 goal *)
Lemma mul_int :
  forall (x:t) (y:t) (m:ieee_float.RoundingMode.mode),
  (is_int x) -> (is_int y) -> (t'isFinite (mul m x y)) -> is_int (mul m x y).
Proof.
  apply mul_int.
Qed.

(* Why3 goal *)
Lemma fma_int : forall (x:t) (y:t) (z:t) (m:ieee_float.RoundingMode.mode),
  (is_int x) -> ((is_int y) -> ((is_int z) -> ((t'isFinite (fma m x y z)) ->
  (is_int (fma m x y z))))).
Proof.
  now apply fma_int.
Qed.

(* Why3 goal *)
Lemma neg_int : forall (x:t), (is_int x) -> is_int (neg x).
Proof.
  apply neg_int.
Qed.

(* Why3 goal *)
Lemma abs_int : forall (x:t), (is_int x) -> is_int (abs x).
Proof.
  apply abs_int.
Qed.

(* Why3 goal *)
Lemma is_int_of_int : forall (x:t) (m:ieee_float.RoundingMode.mode)
  (m':ieee_float.RoundingMode.mode), (is_int x) -> (eq x (of_int m' (to_int m
  x))).
Proof.
  now apply is_int_of_int.
Qed.

(* Why3 goal *)
Lemma is_int_to_int :
  forall (m:ieee_float.RoundingMode.mode) (x:t),
  (is_int x) -> in_int_range (to_int m x).
Proof.
  now apply is_int_to_int.
Qed.

(* Why3 goal *)
Lemma is_int_is_finite : forall (x:t), (is_int x) -> t'isFinite x.
Proof.
  apply is_int_is_finite.
Qed.

(* Why3 goal *)
Lemma int_to_real :
  forall (m:ieee_float.RoundingMode.mode) (x:t),
  (is_int x) -> ((t'real x) = (BuiltIn.IZR (to_int m x))).
Proof.
  apply int_to_real.
Qed.

(* Why3 goal *)
Lemma truncate_int :
  forall (m:ieee_float.RoundingMode.mode) (i:t),
  (is_int i) -> eq (roundToIntegral m i) i.
Proof.
  now apply truncate_int.
Qed.

(* Why3 goal *)
Lemma truncate_neg : forall (x:t), (t'isFinite x) -> ((is_negative x) ->
  ((roundToIntegral ieee_float.RoundingMode.RTZ
  x) = (roundToIntegral ieee_float.RoundingMode.RTP x))).
Proof.
  apply truncate_neg.
Qed.

(* Why3 goal *)
Lemma truncate_pos : forall (x:t), (t'isFinite x) -> ((is_positive x) ->
  ((roundToIntegral ieee_float.RoundingMode.RTZ
  x) = (roundToIntegral ieee_float.RoundingMode.RTN x))).
Proof.
  apply truncate_pos.
Qed.

(* Why3 goal *)
Lemma ceil_le :
  forall (x:t),
  (t'isFinite x) -> le x (roundToIntegral ieee_float.RoundingMode.RTP x).
Proof.
  now apply ceil_le.
Qed.

(* Why3 goal *)
Lemma ceil_lest : forall (x:t) (y:t), ((le x y) /\ (is_int y)) -> (le
  (roundToIntegral ieee_float.RoundingMode.RTP x) y).
Proof.
  now apply ceil_lest.
Qed.

(* Why3 goal *)
Lemma ceil_to_real : forall (x:t), (t'isFinite x) ->
  ((t'real (roundToIntegral ieee_float.RoundingMode.RTP
  x)) = (BuiltIn.IZR (real.Truncate.ceil (t'real x)))).
Proof.
  now apply ceil_to_real.
Qed.

(* Why3 goal *)
Lemma ceil_to_int : forall (m:ieee_float.RoundingMode.mode) (x:t),
  (t'isFinite x) -> ((to_int m (roundToIntegral ieee_float.RoundingMode.RTP
  x)) = (real.Truncate.ceil (t'real x))).
Proof.
  now apply ceil_to_int.
Qed.

(* Why3 goal *)
Lemma floor_le : forall (x:t), (t'isFinite x) -> (le
  (roundToIntegral ieee_float.RoundingMode.RTN x) x).
Proof.
  now apply floor_le.
Qed.

(* Why3 goal *)
Lemma floor_lest :
  forall (x:t) (y:t),
  ((le y x) /\ (is_int y)) ->
  le y (roundToIntegral ieee_float.RoundingMode.RTN x).
Proof.
  now apply floor_lest.
Qed.

(* Why3 goal *)
Lemma floor_to_real : forall (x:t), (t'isFinite x) ->
  ((t'real (roundToIntegral ieee_float.RoundingMode.RTN
  x)) = (BuiltIn.IZR (real.Truncate.floor (t'real x)))).
Proof.
  now apply floor_to_real.
Qed.

(* Why3 goal *)
Lemma floor_to_int : forall (m:ieee_float.RoundingMode.mode) (x:t),
  (t'isFinite x) -> ((to_int m (roundToIntegral ieee_float.RoundingMode.RTN
  x)) = (real.Truncate.floor (t'real x))).
Proof.
  now apply floor_to_int.
Qed.

(* Why3 goal *)
Lemma RNA_down : forall (x:t), (lt (sub ieee_float.RoundingMode.RNE x
  (roundToIntegral ieee_float.RoundingMode.RTN x))
  (sub ieee_float.RoundingMode.RNE
  (roundToIntegral ieee_float.RoundingMode.RTP x) x)) ->
  ((roundToIntegral ieee_float.RoundingMode.RNA
  x) = (roundToIntegral ieee_float.RoundingMode.RTN x)).
Proof.
  apply RNA_down.
Qed.

(* Why3 goal *)
Lemma RNA_up : forall (x:t), (lt (sub ieee_float.RoundingMode.RNE
  (roundToIntegral ieee_float.RoundingMode.RTP x) x)
  (sub ieee_float.RoundingMode.RNE x
  (roundToIntegral ieee_float.RoundingMode.RTN x))) ->
  ((roundToIntegral ieee_float.RoundingMode.RNA
  x) = (roundToIntegral ieee_float.RoundingMode.RTP x)).
Proof.
  apply RNA_up.
Qed.

(* Why3 goal *)
Lemma RNA_down_tie : forall (x:t), (eq (sub ieee_float.RoundingMode.RNE x
  (roundToIntegral ieee_float.RoundingMode.RTN x))
  (sub ieee_float.RoundingMode.RNE
  (roundToIntegral ieee_float.RoundingMode.RTP x) x)) -> ((is_negative x) ->
  ((roundToIntegral ieee_float.RoundingMode.RNA
  x) = (roundToIntegral ieee_float.RoundingMode.RTN x))).
Proof.
  apply RNA_down_tie.
Qed.

(* Why3 goal *)
Lemma RNA_up_tie : forall (x:t), (eq (sub ieee_float.RoundingMode.RNE
  (roundToIntegral ieee_float.RoundingMode.RTP x) x)
  (sub ieee_float.RoundingMode.RNE x
  (roundToIntegral ieee_float.RoundingMode.RTN x))) -> ((is_positive x) ->
  ((roundToIntegral ieee_float.RoundingMode.RNA
  x) = (roundToIntegral ieee_float.RoundingMode.RTP x))).
Proof.
  apply RNA_up_tie.
Qed.

(* Why3 goal *)
Lemma to_int_roundToIntegral :
  forall (m:ieee_float.RoundingMode.mode) (x:t),
  ((to_int m x) = (to_int m (roundToIntegral m x))).
Proof.
  now apply to_int_roundToIntegral.
Qed.

(* Why3 goal *)
Lemma to_int_monotonic : forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  (t'isFinite x) -> ((t'isFinite y) -> ((le x y) -> ((to_int m
  x) <= (to_int m y))%Z)).
Proof.
  apply to_int_monotonic.
Qed.

(* Why3 goal *)
Lemma to_int_of_int :
  forall (m:ieee_float.RoundingMode.mode) (i:Z),
  (in_safe_int_range i) -> ((to_int m (of_int m i)) = i).
Proof.
  intros m i h1.
  now apply to_int_of_int.
Qed.

(* Why3 goal *)
Lemma eq_to_int :
  forall (m:ieee_float.RoundingMode.mode) (x:t) (y:t),
  (t'isFinite x) -> (eq x y) -> ((to_int m x) = (to_int m y)).
Proof.
  apply eq_to_int.
Qed.

(* Why3 goal *)
Lemma neg_to_int :
  forall (m:ieee_float.RoundingMode.mode) (x:t),
  (is_int x) -> ((to_int m (neg x)) = (-(to_int m x))%Z).
Proof.
  apply neg_to_int.
Qed.

(* Why3 goal *)
Lemma roundToIntegral_is_finite :
  forall (m:ieee_float.RoundingMode.mode) (x:t),
  (t'isFinite x) -> t'isFinite (roundToIntegral m x).
Proof.
  now apply roundToIntegral_is_finite.
Qed.

(* Why3 goal *)
Lemma round_bound_ne : forall (x:R), (no_overflow ieee_float.RoundingMode.RNE
  x) ->
  ((((x - ((1 / 16777216)%R * (Reals.Rbasic_fun.Rabs x))%R)%R - (1 / 1427247692705959881058285969449495136382746624)%R)%R <= (round ieee_float.RoundingMode.RNE
  x))%R /\ ((round ieee_float.RoundingMode.RNE
  x) <= ((x + ((1 / 16777216)%R * (Reals.Rbasic_fun.Rabs x))%R)%R + (1 / 1427247692705959881058285969449495136382746624)%R)%R)%R).
intros x h1.
Admitted.

(* Why3 goal *)
Lemma round_bound : forall (m:ieee_float.RoundingMode.mode) (x:R),
  (no_overflow m x) ->
  ((((x - ((1 / 8388608)%R * (Reals.Rbasic_fun.Rabs x))%R)%R - (1 / 713623846352979940529142984724747568191373312)%R)%R <= (round m
  x))%R /\ ((round m
  x) <= ((x + ((1 / 8388608)%R * (Reals.Rbasic_fun.Rabs x))%R)%R + (1 / 713623846352979940529142984724747568191373312)%R)%R)%R).
intros m x h1.
Admitted.

