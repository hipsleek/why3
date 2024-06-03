(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require list.List.
Require list.Length.
Require list.Mem.
Require list.HdTlNoOpt.
Require list.Append.

(* Why3 assumption *)
Inductive tree :=
  | Leaf : tree
  | Node : tree -> tree -> tree.
Axiom tree_WhyType : WhyType tree.
Existing Instance tree_WhyType.

(* Why3 assumption *)
Fixpoint depths (d:Numbers.BinNums.Z)
  (t:tree) {struct t}: Init.Datatypes.list Numbers.BinNums.Z :=
  match t with
  | Leaf => Init.Datatypes.cons d Init.Datatypes.nil
  | Node l r =>
      Init.Datatypes.app (depths (d + 1%Z)%Z l) (depths (d + 1%Z)%Z r)
  end.

Axiom depths_head :
  forall (t:tree) (d:Numbers.BinNums.Z),
  match depths d t with
  | Init.Datatypes.cons x _ => (d <= x)%Z
  | Init.Datatypes.nil => False
  end.

Axiom depths_unique :
  forall (t1:tree) (t2:tree) (d:Numbers.BinNums.Z)
    (s1:Init.Datatypes.list Numbers.BinNums.Z)
    (s2:Init.Datatypes.list Numbers.BinNums.Z),
  ((Init.Datatypes.app (depths d t1) s1) =
   (Init.Datatypes.app (depths d t2) s2)) ->
  (t1 = t2) /\ (s1 = s2).

Axiom depths_prefix :
  forall (t:tree) (d1:Numbers.BinNums.Z) (d2:Numbers.BinNums.Z)
    (s1:Init.Datatypes.list Numbers.BinNums.Z)
    (s2:Init.Datatypes.list Numbers.BinNums.Z),
  ((Init.Datatypes.app (depths d1 t) s1) =
   (Init.Datatypes.app (depths d2 t) s2)) ->
  (d1 = d2).

Axiom depths_prefix_simple :
  forall (t:tree) (d1:Numbers.BinNums.Z) (d2:Numbers.BinNums.Z),
  ((depths d1 t) = (depths d2 t)) -> (d1 = d2).

Axiom depths_subtree :
  forall (t1:tree) (t2:tree) (d1:Numbers.BinNums.Z) (d2:Numbers.BinNums.Z)
    (s1:Init.Datatypes.list Numbers.BinNums.Z),
  ((Init.Datatypes.app (depths d1 t1) s1) = (depths d2 t2)) -> (d2 <= d1)%Z.

Axiom depths_unique2 :
  forall (t1:tree) (t2:tree) (d1:Numbers.BinNums.Z) (d2:Numbers.BinNums.Z),
  ((depths d1 t1) = (depths d2 t2)) -> (d1 = d2) /\ (t1 = t2).

(* Why3 goal *)
Theorem build_rec'vc :
  forall (d:Numbers.BinNums.Z) (s:Init.Datatypes.list Numbers.BinNums.Z),
  forall (x:Numbers.BinNums.Z) (x1:Init.Datatypes.list Numbers.BinNums.Z),
  (s = (Init.Datatypes.cons x x1)) -> ~ (x < d)%Z -> ~ (x = d) ->
  forall (l:tree) (s1:Init.Datatypes.list Numbers.BinNums.Z),
  (s = (Init.Datatypes.app (depths (d + 1%Z)%Z l) s1)) ->
  (forall (t:tree) (s':Init.Datatypes.list Numbers.BinNums.Z),
   ~ ((Init.Datatypes.app (depths (d + 1%Z)%Z t) s') = s1)) ->
  forall (t:tree) (s':Init.Datatypes.list Numbers.BinNums.Z),
  ~ ((Init.Datatypes.app (depths d t) s') = s).
(* Why3 intros d s x x1 h1 h2 h3 l s1 h4 h5 t s'. *)
Proof.
(*intros d s x x1 h1 h2 h3 o o1 h4 h5 t s'.*)
intros d s x x1 h1 h2 h3 result result1 h4 h5 t s'.
subst.
intuition.
destruct t as [_|t1 t2].
(* t = Leaf *)
simpl in H.
injection H.
auto with zarith.
(* t = Node t1 t2 *)
simpl in H.
rewrite <- Append.Append_assoc in H.
rewrite h4 in H.
generalize (depths_unique _ _ _ _ _ H).
intuition.
subst t1.
apply (h5 t2 s'); intuition.
Qed.
