(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require map.Map.
Require map.Occ.
Require map.MapPermut.

(* Why3 assumption *)
Definition unit := unit.

(* Why3 assumption *)
Inductive ref (a:Type) :=
  | mk_ref : a -> ref a.
Axiom ref_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (ref a).
Existing Instance ref_WhyType.
Implicit Arguments mk_ref [[a]].

(* Why3 assumption *)
Definition contents {a:Type} {a_WT:WhyType a} (v:(ref a)): a :=
  match v with
  | (mk_ref x) => x
  end.

(* Why3 assumption *)
Inductive array (a:Type) :=
  | mk_array : Z -> (map.Map.map Z a) -> array a.
Axiom array_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (array a).
Existing Instance array_WhyType.
Implicit Arguments mk_array [[a]].

(* Why3 assumption *)
Definition elts {a:Type} {a_WT:WhyType a} (v:(array a)): (map.Map.map Z a) :=
  match v with
  | (mk_array x x1) => x1
  end.

(* Why3 assumption *)
Definition length {a:Type} {a_WT:WhyType a} (v:(array a)): Z :=
  match v with
  | (mk_array x x1) => x
  end.

(* Why3 assumption *)
Definition get {a:Type} {a_WT:WhyType a} (a1:(array a)) (i:Z): a :=
  (map.Map.get (elts a1) i).

(* Why3 assumption *)
Definition set {a:Type} {a_WT:WhyType a} (a1:(array a)) (i:Z) (v:a): (array
  a) := (mk_array (length a1) (map.Map.set (elts a1) i v)).

(* Why3 assumption *)
Definition map_eq_sub {a:Type} {a_WT:WhyType a} (a1:(map.Map.map Z a))
  (a2:(map.Map.map Z a)) (l:Z) (u:Z): Prop := forall (i:Z), ((l <= i)%Z /\
  (i < u)%Z) -> ((map.Map.get a1 i) = (map.Map.get a2 i)).

(* Why3 assumption *)
Definition array_eq_sub {a:Type} {a_WT:WhyType a} (a1:(array a)) (a2:(array
  a)) (l:Z) (u:Z): Prop := ((length a1) = (length a2)) /\ (((0%Z <= l)%Z /\
  (l <= (length a1))%Z) /\ (((0%Z <= u)%Z /\ (u <= (length a1))%Z) /\
  (map_eq_sub (elts a1) (elts a2) l u))).

(* Why3 assumption *)
Definition array_eq {a:Type} {a_WT:WhyType a} (a1:(array a)) (a2:(array
  a)): Prop := ((length a1) = (length a2)) /\ (map_eq_sub (elts a1) (elts a2)
  0%Z (length a1)).

(* Why3 assumption *)
Definition exchange {a:Type} {a_WT:WhyType a} (a1:(map.Map.map Z a))
  (a2:(map.Map.map Z a)) (l:Z) (u:Z) (i:Z) (j:Z): Prop := ((l <= i)%Z /\
  (i < u)%Z) /\ (((l <= j)%Z /\ (j < u)%Z) /\ (((map.Map.get a1
  i) = (map.Map.get a2 j)) /\ (((map.Map.get a1 j) = (map.Map.get a2 i)) /\
  forall (k:Z), ((l <= k)%Z /\ (k < u)%Z) -> ((~ (k = i)) -> ((~ (k = j)) ->
  ((map.Map.get a1 k) = (map.Map.get a2 k))))))).

Axiom exchange_set : forall {a:Type} {a_WT:WhyType a},
  forall (a1:(map.Map.map Z a)) (l:Z) (u:Z) (i:Z) (j:Z), ((l <= i)%Z /\
  (i < u)%Z) -> (((l <= j)%Z /\ (j < u)%Z) -> (exchange a1
  (map.Map.set (map.Map.set a1 i (map.Map.get a1 j)) j (map.Map.get a1 i)) l
  u i j)).

(* Why3 assumption *)
Definition exchange1 {a:Type} {a_WT:WhyType a} (a1:(array a)) (a2:(array a))
  (i:Z) (j:Z): Prop := ((length a1) = (length a2)) /\ (exchange (elts a1)
  (elts a2) 0%Z (length a1) i j).

(* Why3 assumption *)
Definition permut {a:Type} {a_WT:WhyType a} (a1:(array a)) (a2:(array a))
  (l:Z) (u:Z): Prop := ((length a1) = (length a2)) /\ (((0%Z <= l)%Z /\
  (l <= (length a1))%Z) /\ (((0%Z <= u)%Z /\ (u <= (length a1))%Z) /\
  (map.MapPermut.permut (elts a1) (elts a2) l u))).

(* Why3 assumption *)
Definition permut_sub {a:Type} {a_WT:WhyType a} (a1:(array a)) (a2:(array a))
  (l:Z) (u:Z): Prop := (map_eq_sub (elts a1) (elts a2) 0%Z l) /\ ((permut a1
  a2 l u) /\ (map_eq_sub (elts a1) (elts a2) u (length a1))).

(* Why3 assumption *)
Definition permut_all {a:Type} {a_WT:WhyType a} (a1:(array a)) (a2:(array
  a)): Prop := ((length a1) = (length a2)) /\ (map.MapPermut.permut (elts a1)
  (elts a2) 0%Z (length a1)).

Axiom exchange_permut_sub : forall {a:Type} {a_WT:WhyType a},
  forall (a1:(array a)) (a2:(array a)) (i:Z) (j:Z) (l:Z) (u:Z), (exchange1 a1
  a2 i j) -> (((l <= i)%Z /\ (i < u)%Z) -> (((l <= j)%Z /\ (j < u)%Z) ->
  ((0%Z <= l)%Z -> ((u <= (length a1))%Z -> (permut_sub a1 a2 l u))))).

Axiom permut_sub_weakening : forall {a:Type} {a_WT:WhyType a},
  forall (a1:(array a)) (a2:(array a)) (l1:Z) (u1:Z) (l2:Z) (u2:Z),
  (permut_sub a1 a2 l1 u1) -> (((0%Z <= l2)%Z /\ (l2 <= l1)%Z) ->
  (((u1 <= u2)%Z /\ (u2 <= (length a1))%Z) -> (permut_sub a1 a2 l2 u2))).

Axiom exchange_permut_all : forall {a:Type} {a_WT:WhyType a},
  forall (a1:(array a)) (a2:(array a)) (i:Z) (j:Z), (exchange1 a1 a2 i j) ->
  (permut_all a1 a2).

Axiom elt : Type.
Parameter elt_WhyType : WhyType elt.
Existing Instance elt_WhyType.

Parameter le: elt -> elt -> Prop.

(* Why3 assumption *)
Definition sorted_sub (a:(map.Map.map Z elt)) (l:Z) (u:Z): Prop :=
  forall (i1:Z) (i2:Z), ((l <= i1)%Z /\ ((i1 <= i2)%Z /\ (i2 < u)%Z)) -> (le
  (map.Map.get a i1) (map.Map.get a i2)).

Axiom le_refl : forall (x:elt), (le x x).

Axiom le_asym : forall (x:elt) (y:elt), (~ (le x y)) -> (le y x).

Axiom le_trans : forall (x:elt) (y:elt) (z:elt), ((le x y) /\ (le y z)) ->
  (le x z).

(* Why3 assumption *)
Definition sorted_sub1 (a:(array elt)) (l:Z) (u:Z): Prop := (sorted_sub
  (elts a) l u).

(* Why3 assumption *)
Definition sorted (a:(array elt)): Prop := (sorted_sub (elts a) 0%Z
  (length a)).

Require Import Why3.
Ltac ae := why3 "Alt-Ergo,0.99.1," timelimit 3; admit.

(* Why3 goal *)
Theorem WP_parameter_insertion_sort : forall (a:Z) (a1:(map.Map.map Z elt)),
  let a2 := (mk_array a a1) in ((0%Z <= a)%Z -> let o := (a - 1%Z)%Z in
  ((1%Z <= o)%Z -> forall (a3:(map.Map.map Z elt)), forall (i:Z),
  ((1%Z <= i)%Z /\ (i <= o)%Z) -> (((sorted_sub a3 0%Z i) /\ (permut_all a2
  (mk_array a a3))) -> (((0%Z <= a)%Z /\ ((0%Z <= i)%Z /\ (i < a)%Z)) ->
  let v := (map.Map.get a3 i) in forall (j:Z) (a4:(map.Map.map Z elt)),
  (((0%Z <= j)%Z /\ (j <= i)%Z) /\ ((permut_all a2 (mk_array a
  (map.Map.set a4 j v))) /\ ((forall (k1:Z) (k2:Z), ((0%Z <= k1)%Z /\
  ((k1 <= k2)%Z /\ (k2 <= i)%Z)) -> ((~ (k1 = j)) -> ((~ (k2 = j)) -> (le
  (map.Map.get a4 k1) (map.Map.get a4 k2))))) /\ forall (k:Z),
  (((j + 1%Z)%Z <= k)%Z /\ (k <= i)%Z) -> (le v (map.Map.get a4 k))))) ->
  ((0%Z < j)%Z -> let o1 := (j - 1%Z)%Z in (((0%Z <= a)%Z /\
  ((0%Z <= o1)%Z /\ (o1 < a)%Z)) -> ((~ (le (map.Map.get a4 o1) v)) ->
  let o2 := (j - 1%Z)%Z in (((0%Z <= o2)%Z /\ (o2 < a)%Z) ->
  (((0%Z <= j)%Z /\ (j < a)%Z) -> forall (a5:(map.Map.map Z elt)),
  ((0%Z <= a)%Z /\ (a5 = (map.Map.set a4 j (map.Map.get a4 o2)))) ->
  ((exchange1 (mk_array a (map.Map.set a4 j v)) (mk_array a (map.Map.set a5
  (j - 1%Z)%Z v)) (j - 1%Z)%Z j) -> forall (j1:Z), (j1 = (j - 1%Z)%Z) ->
  (permut_all a2 (mk_array a (map.Map.set a5 j1 v))))))))))))).
(* Why3 intros a a1 a2 h1 o h2 a3 i (h3,h4) (h5,h6) (h7,(h8,h9)) v j a4
        ((h10,h11),(h12,(h13,h14))) h15 o1 (h16,(h17,h18)) h19 o2 (h20,h21)
        (h22,h23) a5 (h24,h25) h26 j1 h27. *)
intros a a1 a2 h1 o h2 a3 i (h3,h4) (h5,h6) (h7,(h8,h9)) v j a4
        ((h10,h11),(h12,(h13,h14))) h15 o1 (h16,(h17,h18)) h19 o2 (h20,h21)
        (h22,h23) a5 (h24,h25) h26 j1 h27.
unfold permut_all in *.
simpl; split; auto.
simpl in h12.
destruct h12 as (h12a & h12b).
apply MapPermut.permut_trans with (1:=h12b).
ae.
Admitted.

