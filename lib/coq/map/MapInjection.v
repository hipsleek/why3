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

(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require HighOrd.
Require int.Int.
Require map.Map.
Require map.Occ.

(* preliminaries *)

Definition into (n:nat) (f:nat -> nat) :=
  forall i:nat, i < n -> f i < n.

Definition injection (n:nat) (f:nat -> nat) :=
  forall i j:nat, i < n -> j < n ->
     f i = f j -> i = j.

Definition surjection (n:nat) (f:nat -> nat) :=
  forall i:nat, i < n ->
    exists j:nat, j < n /\ f j = i.

Require Import Lia Peano_dec.

Theorem injective_implies_surjective:
  forall n:nat,
  forall f:nat -> nat,
  into n f ->
  injection n f ->
  surjection n f.
Proof.
induction n.
(* case n = 0 *)
unfold surjection; intros.
exfalso; lia.
(* case n > 0 *)
intros f Hinto Hinj.
pose (k := f n).
assert (Hbound_k: k < S n) by (apply Hinto; lia).
(* transposition n <-> k *)
pose (trans i :=
  if eq_nat_dec i n then k else
    if eq_nat_dec i k then n else i).
pose (g i := trans (f i)).

(* first step: g maps [0;n[ to [0;n[ *)

assert (Ginto: into n g).
  unfold into, g, trans; intros.
  destruct (eq_nat_dec (f i) n).
  (* f i = n *)
  assert (h : k < n \/ k = n) by lia.
  destruct h; auto.
  exfalso.
  clear g trans; subst.
  assert (f i = i) by (apply Hinj; auto with * ).
  lia.
  (* f i <> n *)
  destruct (eq_nat_dec (f i) k).
  (* f i = k *)
  exfalso.
  assert (i = n) by (apply Hinj; auto with * ).
  lia.
  (* f i <> k *)
  assert (f i < S n) by (apply Hinto; lia).
  lia.

(* second step: trans is injective *)

assert (trans_inj: injection (S n) trans).
  unfold injection, trans; intros.
  destruct (eq_nat_dec i n).
  (* i = n *)
  destruct (eq_nat_dec j n); auto with *.
  (* i = n and j <> n *)
  destruct (eq_nat_dec j k); lia.
  (* i <> n *)
  destruct (eq_nat_dec i k); auto with *.
  destruct (eq_nat_dec j n); auto with *.
  destruct (eq_nat_dec j k); lia.
  (* i <> n and i <> k *)
  destruct (eq_nat_dec j n); auto with *.
  destruct (eq_nat_dec j k); lia.

(* third step: g is injective on [0;n[  *)
assert (Ginj: injection n g).
  red; intros.
  apply Hinj; auto with *.

(* fourth step: g is surjective (by induction hypothesis) *)

assert (Gsurj: surjection n g).
  apply IHn with (f:=g); auto.

(* fifth step: f = trans o g *)

assert (f_is_trans_o_g: forall i, f i = trans (g i)).
  intro i; unfold g, trans.
  destruct (eq_nat_dec (f i) n).
  destruct (eq_nat_dec k n); auto with *.
  destruct (eq_nat_dec k k); auto with *.
  destruct (eq_nat_dec (f i) k).
  destruct (eq_nat_dec n n); auto with *.
  destruct (eq_nat_dec (f i) n); auto with *.
  destruct (eq_nat_dec (f i) k); auto with *.

(* conclusion *)
red; intros.
assert (h: i = k \/ i <> k) by lia.
destruct h.
(* case i = k: the preimage is n *)
exists n; auto.
(* case i <> k *)
assert (h: i = n \/ i <> n) by lia.
destruct h.
(* case i = n: the preimage is the preimage of k by g *)
elim Gsurj with (i:=k).
intros x (h1,h2).
exists x.
split; auto.
rewrite f_is_trans_o_g.
rewrite h2.
unfold trans.
destruct (eq_nat_dec k n); auto with *.
destruct (eq_nat_dec k k); auto with *.
lia.
(* case i <> n and i <> k:
   the preimage is the preimage of i by g
 *)
elim Gsurj with (i:=i).
intros x (h1,h2).
exists x.
split; auto.
rewrite f_is_trans_o_g.
rewrite h2.
unfold trans.
destruct (eq_nat_dec i n); auto with *.
destruct (eq_nat_dec i k); auto with *.
lia.
Qed.



(* lifting the theorem to Z *)

Require Import ZArith.
Open Scope Z_scope.

Theorem lifting:
  forall n:Z,
  forall f:Z -> Z,
    (forall x:Z, 0 <= x < n -> 0 <= f x < n) ->
    exists g:nat -> nat,
      forall i:nat, Z_of_nat i < n -> Z_of_nat (g i) = f (Z_of_nat i).
Proof.
intros n f Hpos.
exists (fun n => Z.abs_nat (f (Z_of_nat n))).
intros i Hi_inf_n.
rewrite inj_Zabs_nat.
rewrite Z.abs_eq; auto.
generalize (Hpos (Z_of_nat i)); auto with *.
Qed.

Theorem Zinjective_implies_surjective:
  forall n:Z,
  forall f:Z -> Z,
  (forall i:Z, 0 <= i < n -> 0 <= f i < n) ->
  (forall i j:Z, 0 <= i < n -> 0 <= j < n -> f i = f j -> i = j) ->
  forall i:Z, 0 <= i < n -> exists k:Z, 0 <= k < n /\ f k = i.
Proof.
intros n f Hinto.
elim (lifting n f Hinto).
intros g Heq_g_f Hinj i Hi_inf_n.
assert (n_pos: 0 <= n) by lia.
elim (Z_of_nat_complete_inf n n_pos).
intros m Heq_n_m.

(* g is into *)

assert (Hinto_g: into m g).
  red; intros i0 Hinter.
  assert (0 <= f (Z_of_nat i0) < n) by (apply Hinto; lia).
  apply inj_lt_rev; auto with *.
  rewrite Heq_g_f; auto with *.

(* g is injective *)

assert (Hinj_g: injection m g).
  red; intros i0 j0 Hinter_i Hinter_j Heq_gi_gj.
  apply inj_eq_rev.
  apply Hinj; auto with *.
  repeat rewrite <- Heq_g_f; auto with *.

(* conclusion *)
generalize (injective_implies_surjective m g Hinto_g Hinj_g).
intro Hsurj_g.
assert (i_pos: 0 <= i) by lia.
elim (Z_of_nat_complete_inf i i_pos).
intros j Heq_j_i.
elim (Hsurj_g j); auto with *.
intros x (inter_x, eq_x).
exists (Z_of_nat x).
split; auto with *.
rewrite <- Heq_g_f; lia.
Qed.




(* Why3 assumption *)
Definition injective (a:Numbers.BinNums.Z -> Numbers.BinNums.Z)
    (n:Numbers.BinNums.Z) : Prop :=
  forall (i:Numbers.BinNums.Z) (j:Numbers.BinNums.Z),
  (0%Z <= i)%Z /\ (i < n)%Z -> (0%Z <= j)%Z /\ (j < n)%Z -> ~ (i = j) ->
  ~ ((a i) = (a j)).

(* Why3 assumption *)
Definition surjective (a:Numbers.BinNums.Z -> Numbers.BinNums.Z)
    (n:Numbers.BinNums.Z) : Prop :=
  forall (i:Numbers.BinNums.Z), (0%Z <= i)%Z /\ (i < n)%Z ->
  exists j:Numbers.BinNums.Z, ((0%Z <= j)%Z /\ (j < n)%Z) /\ ((a j) = i).

(* Why3 assumption *)
Definition range (a:Numbers.BinNums.Z -> Numbers.BinNums.Z)
    (n:Numbers.BinNums.Z) : Prop :=
  forall (i:Numbers.BinNums.Z), (0%Z <= i)%Z /\ (i < n)%Z ->
  (0%Z <= (a i))%Z /\ ((a i) < n)%Z.

(* Why3 goal *)
Lemma injective_surjective :
  forall (a:Numbers.BinNums.Z -> Numbers.BinNums.Z) (n:Numbers.BinNums.Z),
  injective a n -> range a n -> surjective a n.
Proof.
unfold injective, range, surjective.
intros a n h1 h2.
intros.
apply Zinjective_implies_surjective; auto.
intros.
assert (h: (i0 = j \/ i0 <> j)%Z) by lia.
destruct h; auto.
red in h1.
exfalso; apply h1 with i0 j; clear h1; auto.
Qed.

Import Occ.

(* Why3 goal *)
Lemma injection_occ :
  forall (m:Numbers.BinNums.Z -> Numbers.BinNums.Z) (n:Numbers.BinNums.Z),
  injective m n <->
  (forall (v:Numbers.BinNums.Z), ((map.Occ.occ v m 0%Z n) <= 1%Z)%Z).
Proof.
intros m n; split.
(* -> *)
intros inj v.
assert (case: (occ v m 0 n <= 1 \/ occ v m 0 n >= 2)%Z) by lia. destruct case.
trivial.
destruct (occ_exists v m 0 n) as (i,(hi1,hi2)). lia.
assert (0 <= occ v m 0 i)%Z.
  generalize (occ_bounds v m 0 i). lia.
assert (case: (occ v m 0 i = 0 \/ occ v m 0 i > 0)%Z) by lia. destruct case.
assert (0 < occ v m (i+1) n)%Z.
assert (occ v m 0 n = occ v m 0 i + occ v m i n)%Z.
  apply occ_append; lia.
  assert (occ v m i n = occ v m i (i+1) + occ v m (i+1) n)%Z.
  apply occ_append; lia.
  assert (occ v m i (i+1) = 1)%Z.
  rewrite occ_right_add.
  replace (i+1-1)%Z with i by lia.
  rewrite occ_empty; lia.
  lia.
  replace (i+1-1)%Z with i by lia. auto.
  lia.
destruct (occ_exists v m (i+1) n) as (j,(hj1,hj2)). lia.
elim (inj i j); lia.
destruct (occ_exists v m 0 i) as (j,(hj1,hj2)). lia.
elim (inj i j); lia.

(* <- *)
intros Hocc i j hi hj neq eq.
pose (v := m i).
assert (occ v m 0 n >= 2)%Z.
assert (occ v m 0 n = occ v m 0 i + occ v m i n)%Z.
  apply occ_append; lia.
  assert (occ v m i n = occ v m i (i+1) + occ v m (i+1) n)%Z.
  apply occ_append; lia.
  assert (occ v m i (i+1) = 1)%Z.
  rewrite occ_right_add.
  replace (i+1-1)%Z with i by lia.
  rewrite occ_empty; lia.
  lia.
  replace (i+1-1)%Z with i by lia. auto.
assert (case: (j < i \/ i+1 <= j)%Z) by lia. destruct case.
assert (occ v m 0 i >= 1)%Z.
  assert (occ v m 0 i = occ v m 0 j + occ v m j i)%Z.
  apply occ_append; lia.
  assert (occ v m j i = occ v m j (j+1) + occ v m (j+1) i)%Z.
  apply occ_append; lia.
  assert (occ v m j (j+1) = 1)%Z.
  rewrite occ_right_add.
  replace (j+1-1)%Z with j by lia.
  rewrite occ_empty; lia.
  lia.
  replace (j+1-1)%Z with j by lia. auto.
  generalize (occ_bounds v m (i+1) n).
  generalize (occ_bounds v m 0 j).
  generalize (occ_bounds v m (j+1) i).
  lia.
  generalize (occ_bounds v m (i+1) n).
lia.
assert (occ v m (i+1) n >= 1)%Z.
  assert (occ v m (i+1) n = occ v m (i+1) j + occ v m j n)%Z.
  apply occ_append; lia.
  assert (occ v m j n = occ v m j (j+1) + occ v m (j+1) n)%Z.
  apply occ_append; lia.
  assert (occ v m j (j+1) = 1)%Z.
  rewrite occ_right_add.
  replace (j+1-1)%Z with j by lia.
  rewrite occ_empty; lia.
  lia.
  replace (j+1-1)%Z with j by lia. auto.
  generalize (occ_bounds v m (j+1) n).
  generalize (occ_bounds v m 0 i).
  generalize (occ_bounds v m (i+1) j).
  lia.
  generalize (occ_bounds v m 0 i).
  lia.
generalize (Hocc v); lia.
Qed.

