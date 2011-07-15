(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Definition unit  := unit.

Parameter mark : Type.

Parameter at1: forall (a:Type), a -> mark  -> a.

Implicit Arguments at1.

Parameter old: forall (a:Type), a  -> a.

Implicit Arguments old.

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

Parameter length: forall (a:Type), (list a)  -> Z.

Implicit Arguments length.

Axiom length_def : forall (a:Type), forall (l:(list a)),
  match l with
  | Nil  => ((length l) = 0%Z)
  | Cons _ r => ((length l) = (1%Z + (length r))%Z)
  end.

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

Axiom min_dist_equal : forall (w1:(list char)) (w2:(list char)) (a:char)
  (n:Z), (min_dist w1 w2 n) -> (min_dist (Cons a w1) (Cons a w2) n).

Axiom min_dist_diff : forall (w1:(list char)) (w2:(list char)) (a:char)
  (b:char) (m:Z) (p:Z), (~ (a = b)) -> ((min_dist (Cons a w1) w2 p) ->
  ((min_dist w1 (Cons b w2) m) -> (min_dist (Cons a w1) (Cons b w2)
  ((Zmin m p) + 1%Z)%Z))).

Axiom min_dist_eps : forall (w:(list char)) (a:char) (n:Z), (min_dist w
  (Nil:(list char)) n) -> (min_dist (Cons a w) (Nil:(list char))
  (n + 1%Z)%Z).

Axiom min_dist_eps_length : forall (w:(list char)), (min_dist (Nil:(list
  char)) w (length w)).

Inductive ref (a:Type) :=
  | mk_ref : a -> ref a.
Implicit Arguments mk_ref.

Definition contents (a:Type)(u:(ref a)): a :=
  match u with
  | mk_ref contents1 => contents1
  end.
Implicit Arguments contents.

Parameter map : forall (a:Type) (b:Type), Type.

Parameter get: forall (a:Type) (b:Type), (map a b) -> a  -> b.

Implicit Arguments get.

Parameter set: forall (a:Type) (b:Type), (map a b) -> a -> b  -> (map a b).

Implicit Arguments set.

Axiom Select_eq : forall (a:Type) (b:Type), forall (m:(map a b)),
  forall (a1:a) (a2:a), forall (b1:b), (a1 = a2) -> ((get (set m a1 b1)
  a2) = b1).

Axiom Select_neq : forall (a:Type) (b:Type), forall (m:(map a b)),
  forall (a1:a) (a2:a), forall (b1:b), (~ (a1 = a2)) -> ((get (set m a1 b1)
  a2) = (get m a2)).

Parameter const: forall (b:Type) (a:Type), b  -> (map a b).

Set Contextual Implicit.
Implicit Arguments const.
Unset Contextual Implicit.

Axiom Const : forall (b:Type) (a:Type), forall (b1:b) (a1:a), ((get (const(
  b1):(map a b)) a1) = b1).

Inductive array (a:Type) :=
  | mk_array : Z -> (map Z a) -> array a.
Implicit Arguments mk_array.

Definition elts (a:Type)(u:(array a)): (map Z a) :=
  match u with
  | mk_array _ elts1 => elts1
  end.
Implicit Arguments elts.

Definition length1 (a:Type)(u:(array a)): Z :=
  match u with
  | mk_array length2 _ => length2
  end.
Implicit Arguments length1.

Definition get1 (a:Type)(a1:(array a)) (i:Z): a := (get (elts a1) i).
Implicit Arguments get1.

Definition set1 (a:Type)(a1:(array a)) (i:Z) (v:a): (array a) :=
  match a1 with
  | mk_array xcl0 _ => (mk_array xcl0 (set (elts a1) i v))
  end.
Implicit Arguments set1.

Parameter suffix: (array char) -> Z  -> (list char).


Axiom suffix_nil : forall (a:(array char)), ((suffix a
  (length1 a)) = (Nil:(list char))).

Axiom suffix_cons : forall (a:(array char)) (i:Z), ((0%Z <= i)%Z /\
  (i <  (length1 a))%Z) -> ((suffix a i) = (Cons (get1 a i) (suffix a
  (i + 1%Z)%Z))).

Axiom suffix_length : forall (a:(array char)) (i:Z), ((0%Z <= i)%Z /\
  (i <= (length1 a))%Z) -> ((length (suffix a i)) = ((length1 a) - i)%Z).

Definition min_suffix(a1:(array char)) (a2:(array char)) (i:Z) (j:Z)
  (n:Z): Prop := (min_dist (suffix a1 i) (suffix a2 j) n).

Theorem WP_parameter_distance : forall (w1:Z), forall (w2:Z),
  forall (w21:(map Z char)), forall (w11:(map Z char)), let w22 :=
  (mk_array w2 w21) in let w12 := (mk_array w1 w11) in (((0%Z <= w1)%Z /\
  (0%Z <= w2)%Z) -> ((0%Z <= (w2 + 1%Z)%Z)%Z -> ((0%Z <= w2)%Z ->
  forall (t:(map Z Z)), (forall (j:Z), ((0%Z <= j)%Z /\
  (j <  (w2 + 1%Z)%Z)%Z) -> ((get t j) = (w2 - j)%Z)) ->
  ((0%Z <= (w1 - 1%Z)%Z)%Z -> forall (t1:(map Z Z)), forall (i:Z),
  ((i <= (w1 - 1%Z)%Z)%Z /\ (0%Z <= i)%Z) -> ((forall (j:Z), ((0%Z <= j)%Z /\
  (j <= w2)%Z) -> (min_dist (suffix w12 (i + 1%Z)%Z) (suffix w22 j) (get t1
  j))) -> (((0%Z <= w2)%Z /\ (w2 <  (w2 + 1%Z)%Z)%Z) -> (((0%Z <= w2)%Z /\
  (w2 <  (w2 + 1%Z)%Z)%Z) -> (((0%Z <= w2)%Z /\ (w2 <  (w2 + 1%Z)%Z)%Z) ->
  forall (t2:(map Z Z)), (t2 = (set t1 w2 ((get t1 w2) + 1%Z)%Z)) ->
  ((0%Z <= (w2 - 1%Z)%Z)%Z -> forall (oldt:Z), forall (t3:(map Z Z)),
  forall (j:Z), ((j <= (w2 - 1%Z)%Z)%Z /\ (0%Z <= j)%Z) -> (((forall (k:Z),
  ((j <  k)%Z /\ (k <= w2)%Z) -> (min_dist (suffix w12 i) (suffix w22 k)
  (get t3 k))) /\ ((forall (k:Z), ((0%Z <= k)%Z /\ (k <= j)%Z) ->
  (min_dist (suffix w12 (i + 1%Z)%Z) (suffix w22 k) (get t3 k))) /\
  (min_dist (suffix w12 (i + 1%Z)%Z) (suffix w22 (j + 1%Z)%Z) oldt))) ->
  (((0%Z <= j)%Z /\ (j <  (w2 + 1%Z)%Z)%Z) -> forall (oldt1:Z),
  (oldt1 = (get t3 j)) -> (((0%Z <= i)%Z /\ (i <  w1)%Z) -> (((0%Z <= j)%Z /\
  (j <  w2)%Z) -> (((get w11 i) = (get w21 j)) -> (((0%Z <= j)%Z /\
  (j <  (w2 + 1%Z)%Z)%Z) -> forall (t4:(map Z Z)), (t4 = (set t3 j oldt)) ->
  forall (k:Z), (((j + -1%Z)%Z <  k)%Z /\ (k <= w2)%Z) ->
  (min_dist (suffix w12 i) (suffix w22 k) (get t4 k))))))))))))))))).
(* YOU MAY EDIT THE PROOF BELOW *)
intuition.
intuition.
assert (j=k \/ j < k)%Z by omega. intuition.
  (* j=k *)
  subst j.
  rewrite (suffix_cons _ i).
  2: unfold length1; simpl; omega.
  rewrite (suffix_cons _ k).
  2: unfold length1; simpl; omega.
  subst.
  unfold get1; simpl.
  replace (get w11 i) with (get w21 k).
  apply min_dist_equal.
  rewrite Select_eq; auto.
  (* j<k *)
  subst.
  rewrite Select_neq; try omega.
  assert (min_suffix (mk_array w1 w11) (mk_array w2 w21) i k (get t3 k)); auto with *.
unfold min_suffix.
intuition.
Qed.
(* DO NOT EDIT BELOW *)


