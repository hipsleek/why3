(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Axiom Max_is_ge : forall (x:Z) (y:Z), (x <= (Zmax x y))%Z /\
  (y <= (Zmax x y))%Z.

Axiom Max_is_some : forall (x:Z) (y:Z), ((Zmax x y) = x) \/ ((Zmax x y) = y).

Axiom Min_is_le : forall (x:Z) (y:Z), ((Zmin x y) <= x)%Z /\
  ((Zmin x y) <= y)%Z.

Axiom Min_is_some : forall (x:Z) (y:Z), ((Zmin x y) = x) \/ ((Zmin x y) = y).

Axiom Max_x : forall (x:Z) (y:Z), (y <= x)%Z -> ((Zmax x y) = x).

Axiom Max_y : forall (x:Z) (y:Z), (x <= y)%Z -> ((Zmax x y) = y).

Axiom Min_x : forall (x:Z) (y:Z), (x <= y)%Z -> ((Zmin x y) = x).

Axiom Min_y : forall (x:Z) (y:Z), (y <= x)%Z -> ((Zmin x y) = y).

Axiom Max_sym : forall (x:Z) (y:Z), (y <= x)%Z -> ((Zmax x y) = (Zmax y x)).

Axiom Min_sym : forall (x:Z) (y:Z), (y <= x)%Z -> ((Zmin x y) = (Zmin y x)).

Inductive list (a:Type) :=
  | Nil : list a
  | Cons : a -> (list a) -> list a.
Set Contextual Implicit.
Implicit Arguments Nil.
Unset Contextual Implicit.
Implicit Arguments Cons.

Set Implicit Arguments.
Fixpoint length (a:Type)(l:(list a)) {struct l}: Z :=
  match l with
  | Nil  => 0%Z
  | Cons _ r => (1%Z + (length r))%Z
  end.
Unset Implicit Arguments.

Axiom Length_nonnegative : forall (a:Type), forall (l:(list a)),
  (0%Z <= (length l))%Z.

Axiom Length_nil : forall (a:Type), forall (l:(list a)),
  ((length l) = 0%Z) <-> (l = (Nil:(list a))).

Parameter char : Type.

Definition word  := (list char).

Inductive dist : (list char) -> (list char) -> Z -> Prop :=
  | dist_eps : (dist (Nil:(list char)) (Nil:(list char)) 0%Z)
  | dist_add_left : forall (w1:(list char)) (w2:(list char)) (n:Z), (dist w1
      w2 n) -> forall (a:char), (dist (Cons a w1) w2 (n + 1%Z)%Z)
  | dist_add_right : forall (w1:(list char)) (w2:(list char)) (n:Z), (dist w1
      w2 n) -> forall (a:char), (dist w1 (Cons a w2) (n + 1%Z)%Z)
  | dist_context : forall (w1:(list char)) (w2:(list char)) (n:Z), (dist w1
      w2 n) -> forall (a:char), (dist (Cons a w1) (Cons a w2) n).

Definition min_dist(w1:(list char)) (w2:(list char)) (n:Z): Prop := (dist w1
  w2 n) /\ forall (m:Z), (dist w1 w2 m) -> (n <= m)%Z.

Set Implicit Arguments.
Fixpoint infix_plpl (a:Type)(l1:(list a)) (l2:(list a)) {struct l1}: (list
  a) :=
  match l1 with
  | Nil  => l2
  | Cons x1 r1 => (Cons x1 (infix_plpl r1 l2))
  end.
Unset Implicit Arguments.

Axiom Append_assoc : forall (a:Type), forall (l1:(list a)) (l2:(list a))
  (l3:(list a)), ((infix_plpl l1 (infix_plpl l2
  l3)) = (infix_plpl (infix_plpl l1 l2) l3)).

Axiom Append_l_nil : forall (a:Type), forall (l:(list a)), ((infix_plpl l
  (Nil:(list a))) = l).

Axiom Append_length : forall (a:Type), forall (l1:(list a)) (l2:(list a)),
  ((length (infix_plpl l1 l2)) = ((length l1) + (length l2))%Z).

Set Implicit Arguments.
Fixpoint mem (a:Type)(x:a) (l:(list a)) {struct l}: Prop :=
  match l with
  | Nil  => False
  | Cons y r => (x = y) \/ (mem x r)
  end.
Unset Implicit Arguments.

Axiom mem_append : forall (a:Type), forall (x:a) (l1:(list a)) (l2:(list a)),
  (mem x (infix_plpl l1 l2)) <-> ((mem x l1) \/ (mem x l2)).

Axiom mem_decomp : forall (a:Type), forall (x:a) (l:(list a)), (mem x l) ->
  exists l1:(list a), exists l2:(list a), (l = (infix_plpl l1 (Cons x l2))).

Set Implicit Arguments.
Fixpoint last_char(a:char) (u:(list char)) {struct u}: char :=
  match u with
  | Nil  => a
  | Cons c uqt => (last_char c uqt)
  end.
Unset Implicit Arguments.

Set Implicit Arguments.
Fixpoint but_last(a:char) (u:(list char)) {struct u}: (list char) :=
  match u with
  | Nil  => (Nil:(list char))
  | Cons c uqt => (Cons a (but_last c uqt))
  end.
Unset Implicit Arguments.

Axiom first_last_explicit : forall (u:(list char)) (a:char),
  ((infix_plpl (but_last a u) (Cons (last_char a u) (Nil:(list
  char)))) = (Cons a u)).

Axiom first_last : forall (a:char) (u:(list char)), exists v:(list char),
  exists b:char, ((infix_plpl v (Cons b (Nil:(list char)))) = (Cons a u)) /\
  ((length v) = (length u)).

Axiom key_lemma_right : forall (w1:(list char)) (wqt2:(list char)) (m:Z)
  (a:char), (dist w1 wqt2 m) -> forall (w2:(list char)), (wqt2 = (Cons a
  w2)) -> exists u1:(list char), exists v1:(list char), exists k:Z,
  (w1 = (infix_plpl u1 v1)) /\ ((dist v1 w2 k) /\
  ((k + (length u1))%Z <= (m + 1%Z)%Z)%Z).

Axiom dist_symetry : forall (w1:(list char)) (w2:(list char)) (n:Z), (dist w1
  w2 n) -> (dist w2 w1 n).

Axiom key_lemma_left : forall (w1:(list char)) (w2:(list char)) (m:Z)
  (a:char), (dist (Cons a w1) w2 m) -> exists u2:(list char), exists v2:(list
  char), exists k:Z, (w2 = (infix_plpl u2 v2)) /\ ((dist w1 v2 k) /\
  ((k + (length u2))%Z <= (m + 1%Z)%Z)%Z).

Axiom dist_concat_left : forall (u:(list char)) (v:(list char)) (w:(list
  char)) (n:Z), (dist v w n) -> (dist (infix_plpl u v) w ((length u) + n)%Z).

Axiom dist_concat_right : forall (u:(list char)) (v:(list char)) (w:(list
  char)) (n:Z), (dist v w n) -> (dist v (infix_plpl u w) ((length u) + n)%Z).

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Theorem min_dist_equal : forall (w1:(list char)) (w2:(list char)) (a:char)
  (n:Z), (min_dist w1 w2 n) -> (min_dist (Cons a w1) (Cons a w2) n).
(* YOU MAY EDIT THE PROOF BELOW *)
intros w1 w2 a n.
unfold min_dist.
generalize dist_context; intuition.

inversion H0.

elim (key_lemma_right w1 (Cons a w2) n0 a H7 w2);
 [ idtac | reflexivity ].
intros u1 Hex; elim Hex; clear Hex.
intros v1 Hex; elim Hex; clear Hex.
intros k Hex.
 decompose [and] Hex; clear Hex.
generalize (dist_concat_left u1 v1 w2 k H10); intro.
apply Zle_trans with (length u1 + k)%Z.
apply H2.
rewrite H8; assumption.
omega.
elim (key_lemma_left w1 w2 n0 a H7).
intros u2 Hex; elim Hex; clear Hex.
intros v2 Hex; elim Hex; clear Hex.
intros k Hex.
 decompose [and] Hex; clear Hex.
generalize (dist_concat_right u2 w1 v2 k H10); intro.
apply Zle_trans with (length u2 + k)%Z.
apply H2.
rewrite H8; assumption.
omega.
auto.
Qed.
(* DO NOT EDIT BELOW *)


