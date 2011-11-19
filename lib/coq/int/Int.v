(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
(*Add Rec LoadPath "/home/guillaume/bin/why3/share/why3/theories".*)
(*Add Rec LoadPath "/home/guillaume/bin/why3/share/why3/modules".*)

Notation infix_ls := Zlt (only parsing).

Definition infix_lseq(x:Z) (y:Z): Prop := (infix_ls x y) \/ (x = y).

Lemma infix_lseq_Zle :
  forall x y, infix_lseq x y <-> Zle x y.
Proof.
intros x y.
apply iff_Symmetric.
apply Zle_lt_or_eq_iff.
Qed.

Notation infix_pl := Zplus (only parsing).

Notation prefix_mn := Zopp (only parsing).

Notation infix_as := Zmult (only parsing).

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Unit_def : forall (x:Z), ((infix_pl x 0%Z) = x).
(* YOU MAY EDIT THE PROOF BELOW *)
exact Zplus_0_r.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Assoc : forall (x:Z) (y:Z) (z:Z), ((infix_pl (infix_pl x y)
  z) = (infix_pl x (infix_pl y z))).
(* YOU MAY EDIT THE PROOF BELOW *)
intros x y z.
apply sym_eq.
apply Zplus_assoc.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Inv_def : forall (x:Z), ((infix_pl x (prefix_mn x)) = 0%Z).
(* YOU MAY EDIT THE PROOF BELOW *)
exact Zplus_opp_r.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Comm : forall (x:Z) (y:Z), ((infix_pl x y) = (infix_pl y x)).
(* YOU MAY EDIT THE PROOF BELOW *)
exact Zplus_comm.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Assoc1 : forall (x:Z) (y:Z) (z:Z), ((infix_as (infix_as x y)
  z) = (infix_as x (infix_as y z))).
(* YOU MAY EDIT THE PROOF BELOW *)
intros x y z.
apply sym_eq.
apply Zmult_assoc.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Mul_distr : forall (x:Z) (y:Z) (z:Z), ((infix_as x (infix_pl y
  z)) = (infix_pl (infix_as x y) (infix_as x z))).
(* YOU MAY EDIT THE PROOF BELOW *)
exact Zmult_plus_distr_r.
Qed.
(* DO NOT EDIT BELOW *)

Definition infix_mn(x:Z) (y:Z): Z := (infix_pl x (prefix_mn y)).

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Comm1 : forall (x:Z) (y:Z), ((infix_as x y) = (infix_as y x)).
(* YOU MAY EDIT THE PROOF BELOW *)
exact Zmult_comm.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Unitary : forall (x:Z), ((infix_as 1%Z x) = x).
(* YOU MAY EDIT THE PROOF BELOW *)
exact Zmult_1_l.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma NonTrivialRing : ~ (0%Z = 1%Z).
(* YOU MAY EDIT THE PROOF BELOW *)
discriminate.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Refl : forall (x:Z), (infix_lseq x x).
(* YOU MAY EDIT THE PROOF BELOW *)
intros x.
apply infix_lseq_Zle.
apply Zle_refl.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Trans : forall (x:Z) (y:Z) (z:Z), (infix_lseq x y) -> ((infix_lseq y
  z) -> (infix_lseq x z)).
(* YOU MAY EDIT THE PROOF BELOW *)
intros x y z H1 H2.
apply infix_lseq_Zle.
apply Zle_trans with y ;
  now apply infix_lseq_Zle.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Antisymm : forall (x:Z) (y:Z), (infix_lseq x y) -> ((infix_lseq y x) ->
  (x = y)).
(* YOU MAY EDIT THE PROOF BELOW *)
intros x y H1 H2.
apply Zle_antisym ;
  now apply infix_lseq_Zle.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma Total : forall (x:Z) (y:Z), (infix_lseq x y) \/ (infix_lseq y x).
(* YOU MAY EDIT THE PROOF BELOW *)
intros x y.
destruct (Zle_or_lt x y) as [H|H].
left.
now apply infix_lseq_Zle.
right.
apply infix_lseq_Zle.
now apply Zlt_le_weak.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma CompatOrderAdd : forall (x:Z) (y:Z) (z:Z), (infix_lseq x y) ->
  (infix_lseq (infix_pl x z) (infix_pl y z)).
(* YOU MAY EDIT THE PROOF BELOW *)
intros x y z H.
apply infix_lseq_Zle.
apply Zplus_le_compat_r.
now apply infix_lseq_Zle.
Qed.
(* DO NOT EDIT BELOW *)

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Lemma CompatOrderMult : forall (x:Z) (y:Z) (z:Z), (infix_lseq x y) ->
  ((infix_lseq 0%Z z) -> (infix_lseq (infix_as x z) (infix_as y z))).
(* YOU MAY EDIT THE PROOF BELOW *)
intros x y z H1 H2.
apply infix_lseq_Zle.
apply Zmult_le_compat_r ;
  now apply infix_lseq_Zle.
Qed.
(* DO NOT EDIT BELOW *)


