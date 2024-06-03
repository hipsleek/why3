(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require list.List.
Require list.Length.
Require list.Mem.
Require list.Nth.
Require option.Option.
Require list.NthLength.
Require list.Append.
Require list.NthLengthAppend.

(* Why3 assumption *)
Definition pal {a:Type} {a_WT:WhyType a} (x:Init.Datatypes.list a)
    (n:Numbers.BinNums.Z) : Prop :=
  forall (i:Numbers.BinNums.Z), (0%Z <= i)%Z /\ (i < n)%Z ->
  ((list.Nth.nth i x) = (list.Nth.nth ((n - 1%Z)%Z - i)%Z x)).

Axiom elt : Type.
Parameter elt_WhyType : WhyType elt.
Existing Instance elt_WhyType.

Parameter eq: elt -> elt -> Prop.

Axiom eq'spec : forall (x:elt) (y:elt), eq x y <-> (x = y).

(* Why3 goal *)
Theorem palindrome_rec'vc :
  forall (x:Init.Datatypes.list elt) (y:Init.Datatypes.list elt),
  ((list.Length.length y) <= (list.Length.length x))%Z ->
  forall (x1:elt) (x2:Init.Datatypes.list elt),
  (y = (Init.Datatypes.cons x1 x2)) ->
  forall (x3:elt) (x4:Init.Datatypes.list elt),
  (x2 = (Init.Datatypes.cons x3 x4)) ->
  forall (x5:elt) (x6:Init.Datatypes.list elt),
  (x = (Init.Datatypes.cons x5 x6)) ->
  (exists i:Numbers.BinNums.Z,
   ((0%Z <= i)%Z /\ (i < (list.Length.length x4))%Z) /\
   ~ ((list.Nth.nth i x6) =
      (list.Nth.nth (((list.Length.length x4) - 1%Z)%Z - i)%Z x6))) ->
  exists i:Numbers.BinNums.Z,
  ((0%Z <= i)%Z /\ (i < (list.Length.length y))%Z) /\
  ~ ((list.Nth.nth i x) =
     (list.Nth.nth (((list.Length.length y) - 1%Z)%Z - i)%Z x)).
(* Why3 intros x y h1 x1 x2 h2 x3 x4 h3 x5 x6 h4 (i,((h5,h6),h7)). *)
Proof.
intros x y h1 x1 x2 h2 x3 x4 h3 x5 x6 h4 (i,(hi1,hi2)).
subst.
exists (i+1)%Z; intuition.
unfold Length.length. fold Length.length.
auto with zarith.
unfold Length.length in *. fold Length.length in *.
assert (Nth.nth (i+1) (x5 :: x6) = Nth.nth i x6).
  unfold Nth.nth; fold Nth.nth.
  generalize (Zeq_bool_eq (i+1) 0).
  destruct (Zeq_bool (i+1) 0).
  intuition.
  elimtype False.
  auto with zarith.
  intuition.
  replace (i+1-1)%Z with i by auto with zarith. auto.
replace (1 + (1 + Length.length x4) - 1 - (i + 1))%Z
         with (1 + Length.length x4 - 1 - i)%Z
  in H1 by auto with zarith.
assert (Nth.nth (1 + Length.length x4 - 1 - i) (x5 :: x6) =
        Nth.nth (Length.length x4 - 1 - i) x6).
    unfold Nth.nth; fold Nth.nth.
  generalize (Zeq_bool_eq (1 + Length.length x4 - 1 - i) 0).
    destruct (Zeq_bool (1 + Length.length x4 - 1 - i) 0).
  intuition; elimtype False; auto with zarith.
  intuition.
  replace (1 + Length.length x4 - 1 - i - 1)%Z with (Length.length x4 - 1 - i)%Z
  by auto with zarith; auto.
congruence.
Qed.

