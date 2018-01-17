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
Require int.Int.
Require map.Map.

(* Why3 goal *)
Definition occ: forall {a:Type} {a_WT:WhyType a}, a -> (map.Map.map Z a) ->
  Z -> Z -> Z.
Proof.
intros a a_WT v m l u.
induction (Z.to_nat (u-l)) as [|delta occ_].
exact Z0.
exact ((if why_decidable_eq (map.Map.get m (l + Z_of_nat delta)%Z) v then 1 else 0) + occ_)%Z.
Defined.

Lemma occ_equation :
  forall {a:Type} {a_WT:WhyType a} v m l u,
  (l < u)%Z ->
  occ v m l u =
  ((if why_decidable_eq (map.Map.get m (u - 1)%Z) v then 1 else 0) + occ v m l (u - 1))%Z.
Proof.
intros a a_WT v m l u Hlu.
assert (0 < u - l)%Z as h1' by omega.
unfold occ.
replace (u - 1 - l)%Z with (u - l - 1)%Z by ring.
replace (u - 1)%Z with (l + (u - l - 1))%Z by ring.
rewrite <- (Z2Nat.id (u - l - 1)) by omega.
rewrite (Z2Nat.inj_sub _ 1) by easy.
destruct (u - l)%Z ; try easy.
simpl.
assert (exists n, Pos.to_nat p = S n) as [n ->].
  exists (Z.to_nat (Zpred (Zpos p))).
  rewrite Z2Nat.inj_pred.
  apply (S_pred _ O).
  apply Pos2Nat.is_pos.
simpl.
now rewrite <- minus_n_O, Nat2Z.id.
Qed.

(* Why3 goal *)
Lemma occ_empty :
forall {a:Type} {a_WT:WhyType a},
forall (v:a) (m:(map.Map.map Z a)) (l:Z) (u:Z),
 (u <= l)%Z -> ((occ v m l u) = 0%Z).
Proof.
intros a a_WT v m l u h1.
assert (u - l <= 0)%Z as h1' by omega.
unfold occ.
destruct (u - l)%Z ; try reflexivity.
now elim h1'.
Qed.

(* Why3 goal *)
Lemma occ_right_no_add :
forall {a:Type} {a_WT:WhyType a},
forall (v:a) (m:(map.Map.map Z a)) (l:Z) (u:Z),
 (l < u)%Z ->
 ((~ ((map.Map.get m (u - 1%Z)%Z) = v)) ->
  ((occ v m l u) = (occ v m l (u - 1%Z)%Z))).
Proof.
intros a a_WT v m l u h1 h2.
rewrite occ_equation with (1 := h1).
now destruct why_decidable_eq as [H|H].
Qed.

(* Why3 goal *)
Lemma occ_right_add :
forall {a:Type} {a_WT:WhyType a},
forall (v:a) (m:(map.Map.map Z a)) (l:Z) (u:Z),
 (l < u)%Z ->
 (((map.Map.get m (u - 1%Z)%Z) = v) ->
  ((occ v m l u) = (1%Z + (occ v m l (u - 1%Z)%Z))%Z)).
Proof.
intros a a_WT v m l u h1 h2.
rewrite occ_equation with (1 := h1).
now destruct why_decidable_eq as [H|H].
Qed.

(* Why3 goal *)
Lemma occ_bounds :
forall {a:Type} {a_WT:WhyType a},
forall (v:a) (m:(map.Map.map Z a)) (l:Z) (u:Z),
 (l <= u)%Z -> ((0%Z <= (occ v m l u))%Z /\ ((occ v m l u) <= (u - l)%Z)%Z).
Proof.
intros a a_WT v m l u h1.
cut (0 <= u - l)%Z. 2: omega.
replace (occ v m l u) with (occ v m l (l + (u - l)))%Z.
pattern (u - l)%Z; apply Z_lt_induction. 2: omega.
intros.
assert (h: (x = 0 \/ x <> 0)%Z) by omega. destruct h.
now rewrite occ_empty; omega.
destruct (why_decidable_eq (Map.get m (l + (x-1))%Z) v).
rewrite occ_right_add.
generalize (H (x-1)%Z); clear H; intros.
assert (0 <= occ v m l (l + (x - 1)) <= x-1)%Z.
apply H; omega.
replace (l + x - 1)%Z with (l+(x-1))%Z by ring.
omega.
omega.
replace (l + x - 1)%Z with (l+(x-1))%Z by ring.
trivial.
rewrite occ_right_no_add.
assert (0 <= occ v m l (l + (x - 1)) <= x-1)%Z.
apply H; omega.
replace (l + x - 1)%Z with (l+(x-1))%Z by ring.
omega.
omega.
replace (l + x - 1)%Z with (l+(x-1))%Z by ring.
trivial.
replace (l + (u-l))%Z with u by ring. trivial.
Qed.

(* Why3 goal *)
Lemma occ_append :
forall {a:Type} {a_WT:WhyType a},
forall (v:a) (m:(map.Map.map Z a)) (l:Z) (mid:Z) (u:Z),
 ((l <= mid)%Z /\ (mid <= u)%Z) ->
 ((occ v m l u) = ((occ v m l mid) + (occ v m mid u))%Z).
Proof.
intros a a_WT v m l mid u (h1,h2).
cut (0 <= u - mid)%Z. 2: omega.
replace (occ v m l u) with (occ v m l (mid + (u - mid)))%Z.
replace (occ v m mid u) with (occ v m mid (mid + (u - mid)))%Z.
pattern (u - mid)%Z; apply Z_lt_induction. 2: omega.
intros.
assert (h: (x = 0 \/ x <> 0)%Z) by omega. destruct h.
rewrite (occ_empty _ _ mid (mid+x)%Z).
subst x. ring_simplify ((mid+0)%Z). ring.
omega.
destruct (why_decidable_eq (Map.get m (mid + (x-1))%Z) v).
rewrite (occ_right_add _ _ l (mid+x))%Z.
rewrite (occ_right_add _ _ mid (mid+x))%Z.
generalize (H (x-1)%Z); clear H; intros.
assert ((occ v m l (mid+(x-1)) = (occ v m l mid) + occ v m mid (mid + (x - 1)))%Z).
apply H; omega.
replace (mid + x - 1)%Z with (mid+(x-1))%Z by ring.
omega. omega.
trivial.
replace (mid + x - 1)%Z with (mid+(x-1))%Z by ring. trivial.
omega.
replace (mid + x - 1)%Z with (mid+(x-1))%Z by ring. trivial.

rewrite (occ_right_no_add _ _ l (mid+x))%Z.
rewrite (occ_right_no_add _ _ mid (mid+x))%Z.
generalize (H (x-1)%Z); clear H; intros.
assert ((occ v m l (mid+(x-1)) = (occ v m l mid) + occ v m mid (mid + (x - 1)))%Z).
apply H; omega.
replace (mid + x - 1)%Z with (mid+(x-1))%Z by ring.
omega. omega.
trivial.
replace (mid + x - 1)%Z with (mid+(x-1))%Z by ring. trivial.
omega.
replace (mid + x - 1)%Z with (mid+(x-1))%Z by ring. trivial.

replace (mid + (u-mid))%Z with u by ring. trivial.
replace (mid + (u-mid))%Z with u by ring. trivial.
Qed.

(* Why3 goal *)
Lemma occ_neq :
forall {a:Type} {a_WT:WhyType a},
forall (v:a) (m:(map.Map.map Z a)) (l:Z) (u:Z),
 (forall (i:Z), ((l <= i)%Z /\ (i < u)%Z) -> ~ ((map.Map.get m i) = v)) ->
 ((occ v m l u) = 0%Z).
Proof.
intros a a_WT v m l u.
assert (h: (u < l \/ 0 <= u - l)%Z) by omega. destruct h.
rewrite occ_empty. trivial. omega.
replace u with (l + (u - l))%Z. 2:ring.
generalize H.
pattern (u - l)%Z; apply Z_lt_induction. 2: omega.
clear H; intros.
assert (h: (x = 0 \/ x <> 0)%Z) by omega. destruct h.
now rewrite occ_empty; omega.
destruct (why_decidable_eq (Map.get m (l + (x-1))%Z) v).
assert (Map.get m (l + (x - 1)) <> v)%Z.
  apply H1; omega.
intuition.
rewrite occ_right_no_add.
replace (l+x-1)%Z with (l+(x-1))%Z by ring.
apply H; intuition.
apply (H1 i). omega. assumption.
omega.
replace (l + x - 1)%Z with (l+(x-1))%Z by ring.
trivial.
Qed.

(* Why3 goal *)
Lemma occ_exists :
forall {a:Type} {a_WT:WhyType a},
forall (v:a) (m:(map.Map.map Z a)) (l:Z) (u:Z),
 (0%Z < (occ v m l u))%Z ->
 exists i:Z, ((l <= i)%Z /\ (i < u)%Z) /\ ((map.Map.get m i) = v).
Proof.
intros a a_WT v m l u h1.
assert (h: (u < l \/ 0 <= u - l)%Z) by omega. destruct h.
rewrite occ_empty in h1. elimtype False; omega. omega.
generalize h1.
replace u with (l + (u - l))%Z. 2:ring.
generalize H.
pattern (u - l)%Z; apply Z_lt_induction. 2: omega.
clear H; intros.
assert (h: (x = 0 \/ x <> 0)%Z) by omega. destruct h.
rewrite occ_empty in h0. elimtype False; omega. omega.
destruct (why_decidable_eq (Map.get m (l + (x-1))%Z) v).
exists (l+(x-1))%Z. split. omega. now trivial.
destruct (H (x-1))%Z as (i,(hi1,hi2)). omega. omega.
rewrite occ_right_no_add in h0.
replace (l + (x - 1))%Z with (l+x-1)%Z by ring. trivial.
omega.
replace (l + x - 1)%Z with (l+(x-1))%Z by ring. trivial.
exists i. split. omega. assumption.
Qed.

(* Why3 goal *)
Lemma occ_pos :
forall {a:Type} {a_WT:WhyType a},
forall (m:(map.Map.map Z a)) (l:Z) (u:Z) (i:Z),
 ((l <= i)%Z /\ (i < u)%Z) -> (0%Z < (occ (map.Map.get m i) m l u))%Z.
Proof.
intros a a_WT m l u i (h1,h2).
pose (v := (Map.get m i)). fold v.
assert (occ v m l u = occ v m l i + occ v m i u)%Z.
  apply occ_append. omega.
assert (occ v m i u = occ v m i (i+1) + occ v m (i+1) u)%Z.
  apply occ_append. omega.
assert (occ v m i (i + 1) = 1)%Z.
rewrite occ_right_add.
  ring_simplify (i+1-1)%Z. rewrite occ_empty. ring. omega. omega.
ring_simplify (i+1-1)%Z. auto.
assert (0 <= occ v m l i <= i -l)%Z. apply occ_bounds. omega.
assert (0 <= occ v m i (i+1) <= (i+1)-i)%Z. apply occ_bounds. omega.
assert (0 <= occ v m (i+1) u <= u - (i+1))%Z. apply occ_bounds. omega.
omega.
Qed.

(* Why3 goal *)
Lemma occ_eq :
forall {a:Type} {a_WT:WhyType a},
forall (v:a) (m1:(map.Map.map Z a)) (m2:(map.Map.map Z a)) (l:Z) (u:Z),
 (forall (i:Z),
   ((l <= i)%Z /\ (i < u)%Z) -> ((map.Map.get m1 i) = (map.Map.get m2 i))) ->
 ((occ v m1 l u) = (occ v m2 l u)).
Proof.
intros a a_WT v m1 m2 l u h1.
assert (h: (u < l \/ 0 <= u - l)%Z) by omega. destruct h.
rewrite occ_empty.
rewrite occ_empty. trivial.
omega. omega.
generalize h1.
replace u with (l + (u - l))%Z. 2:ring.
generalize H.
pattern (u - l)%Z; apply Z_lt_induction. 2: omega.
clear H; intros.
assert (h: (x = 0 \/ x <> 0)%Z) by omega. destruct h.
rewrite occ_empty. rewrite occ_empty. trivial. omega. omega.
destruct (why_decidable_eq (Map.get m1 (l + (x-1))%Z) v).
rewrite occ_right_add.
rewrite (occ_right_add v m2).
apply f_equal.
replace (l + x - 1)%Z with (l+(x-1))%Z by ring.
apply H. omega. omega. intros. apply h0. omega. omega.
replace (l + x - 1)%Z with (l+(x-1))%Z by ring.
rewrite <- h0. trivial. omega. omega.
replace (l + x - 1)%Z with (l+(x-1))%Z by ring. assumption.

rewrite occ_right_no_add.
rewrite (occ_right_no_add v m2).
replace (l + x - 1)%Z with (l+(x-1))%Z by ring.
apply H. omega. omega. intros. apply h0. omega. omega.
replace (l + x - 1)%Z with (l+(x-1))%Z by ring.
rewrite <- h0. trivial. omega. omega.
replace (l + x - 1)%Z with (l+(x-1))%Z by ring. assumption.
Qed.

