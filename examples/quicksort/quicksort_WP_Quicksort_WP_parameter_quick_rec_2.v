(* This file is generated by Why3's Coq 8.4 driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require map.Map.
Require map.MapPermut.

(* Why3 assumption *)
Definition unit := unit.

(* Why3 assumption *)
Inductive ref (a:Type) {a_WT:WhyType a} :=
  | mk_ref : a -> ref a.
Axiom ref_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (ref a).
Existing Instance ref_WhyType.
Implicit Arguments mk_ref [[a] [a_WT]].

(* Why3 assumption *)
Definition contents {a:Type} {a_WT:WhyType a} (v:(@ref a a_WT)): a :=
  match v with
  | (mk_ref x) => x
  end.

(* Why3 assumption *)
Inductive array
  (a:Type) {a_WT:WhyType a} :=
  | mk_array : Z -> (@map.Map.map Z _ a a_WT) -> array a.
Axiom array_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (array a).
Existing Instance array_WhyType.
Implicit Arguments mk_array [[a] [a_WT]].

(* Why3 assumption *)
Definition elts {a:Type} {a_WT:WhyType a} (v:(@array a a_WT)): (@map.Map.map
  Z _ a a_WT) := match v with
  | (mk_array x x1) => x1
  end.

(* Why3 assumption *)
Definition length {a:Type} {a_WT:WhyType a} (v:(@array a a_WT)): Z :=
  match v with
  | (mk_array x x1) => x
  end.

(* Why3 assumption *)
Definition get {a:Type} {a_WT:WhyType a} (a1:(@array a a_WT)) (i:Z): a :=
  (map.Map.get (elts a1) i).

(* Why3 assumption *)
Definition set {a:Type} {a_WT:WhyType a} (a1:(@array a a_WT)) (i:Z)
  (v:a): (@array a a_WT) := (mk_array (length a1) (map.Map.set (elts a1) i
  v)).

(* Why3 assumption *)
Definition make {a:Type} {a_WT:WhyType a} (n:Z) (v:a): (@array a a_WT) :=
  (mk_array n (map.Map.const v:(@map.Map.map Z _ a a_WT))).

(* Why3 assumption *)
Definition sorted_sub (a:(@map.Map.map Z _ Z _)) (l:Z) (u:Z): Prop :=
  forall (i1:Z) (i2:Z), (((l <= i1)%Z /\ (i1 <= i2)%Z) /\ (i2 < u)%Z) ->
  ((map.Map.get a i1) <= (map.Map.get a i2))%Z.

(* Why3 assumption *)
Definition sorted_sub1 (a:(@array Z _)) (l:Z) (u:Z): Prop := (sorted_sub
  (elts a) l u).

(* Why3 assumption *)
Definition sorted (a:(@array Z _)): Prop := (sorted_sub (elts a) 0%Z
  (length a)).

(* Why3 assumption *)
Definition exchange {a:Type} {a_WT:WhyType a} (a1:(@array a a_WT))
  (a2:(@array a a_WT)) (i:Z) (j:Z): Prop := (map.MapPermut.exchange (elts a1)
  (elts a2) i j).

(* Why3 assumption *)
Definition permut_sub {a:Type} {a_WT:WhyType a} (a1:(@array a a_WT))
  (a2:(@array a a_WT)) (l:Z) (u:Z): Prop := (map.MapPermut.permut_sub
  (elts a1) (elts a2) l u).

(* Why3 assumption *)
Definition permut {a:Type} {a_WT:WhyType a} (a1:(@array a a_WT)) (a2:(@array
  a a_WT)): Prop := ((length a1) = (length a2)) /\ (map.MapPermut.permut_sub
  (elts a1) (elts a2) 0%Z (length a1)).

Axiom exchange_permut : forall {a:Type} {a_WT:WhyType a}, forall (a1:(@array
  a a_WT)) (a2:(@array a a_WT)) (i:Z) (j:Z), (exchange a1 a2 i j) ->
  (((length a1) = (length a2)) -> (((0%Z <= i)%Z /\ (i < (length a1))%Z) ->
  (((0%Z <= j)%Z /\ (j < (length a1))%Z) -> (permut a1 a2)))).

Axiom permut_sym : forall {a:Type} {a_WT:WhyType a}, forall (a1:(@array
  a a_WT)) (a2:(@array a a_WT)), (permut a1 a2) -> (permut a2 a1).

Axiom permut_trans : forall {a:Type} {a_WT:WhyType a}, forall (a1:(@array
  a a_WT)) (a2:(@array a a_WT)) (a3:(@array a a_WT)), (permut a1 a2) ->
  ((permut a2 a3) -> (permut a1 a3)).

(* Why3 assumption *)
Definition map_eq_sub {a:Type} {a_WT:WhyType a} (a1:(@map.Map.map Z _
  a a_WT)) (a2:(@map.Map.map Z _ a a_WT)) (l:Z) (u:Z): Prop := forall (i:Z),
  ((l <= i)%Z /\ (i < u)%Z) -> ((map.Map.get a1 i) = (map.Map.get a2 i)).

(* Why3 assumption *)
Definition array_eq_sub {a:Type} {a_WT:WhyType a} (a1:(@array a a_WT))
  (a2:(@array a a_WT)) (l:Z) (u:Z): Prop := (map_eq_sub (elts a1) (elts a2) l
  u).

(* Why3 assumption *)
Definition array_eq {a:Type} {a_WT:WhyType a} (a1:(@array a a_WT))
  (a2:(@array a a_WT)): Prop := ((length a1) = (length a2)) /\ (array_eq_sub
  a1 a2 0%Z (length a1)).

Axiom array_eq_sub_permut : forall {a:Type} {a_WT:WhyType a},
  forall (a1:(@array a a_WT)) (a2:(@array a a_WT)) (l:Z) (u:Z), (array_eq_sub
  a1 a2 l u) -> (permut_sub a1 a2 l u).

Axiom array_eq_permut : forall {a:Type} {a_WT:WhyType a}, forall (a1:(@array
  a a_WT)) (a2:(@array a a_WT)), (array_eq a1 a2) -> (permut a1 a2).

Import MapPermut.

(* Why3 goal *)
Theorem WP_parameter_quick_rec : forall (t:Z) (t1:(@map.Map.map Z _ Z _))
  (l:Z) (r:Z), ((0%Z <= t)%Z /\ ((0%Z <= l)%Z /\ (r < t)%Z)) -> ((l < r)%Z ->
  (((0%Z <= l)%Z /\ (l < t)%Z) -> let v := (map.Map.get t1 l) in
  (((l + 1%Z)%Z <= r)%Z -> forall (m:Z) (t2:(@map.Map.map Z _ Z _)),
  ((forall (j:Z), ((l < j)%Z /\ (j <= m)%Z) -> ((map.Map.get t2 j) < v)%Z) /\
  ((forall (j:Z), ((m < j)%Z /\ (j < (r + 1%Z)%Z)%Z) -> (v <= (map.Map.get t2
  j))%Z) /\ ((map.MapPermut.permut_sub t1 t2 l (r + 1%Z)%Z) /\
  (((map.Map.get t2 l) = v) /\ ((l <= m)%Z /\ (m < (r + 1%Z)%Z)%Z))))) ->
  (((0%Z <= t)%Z /\ (((0%Z <= l)%Z /\ (l < t)%Z) /\ ((0%Z <= m)%Z /\
  (m < t)%Z))) -> forall (t3:(@map.Map.map Z _ Z _)), ((0%Z <= t)%Z /\
  (map.MapPermut.exchange t2 t3 l m)) -> let o := (m - 1%Z)%Z in
  (((0%Z <= l)%Z /\ (o < t)%Z) -> forall (t4:(@map.Map.map Z _ Z _)),
  ((0%Z <= t)%Z /\ ((sorted_sub t4 l (o + 1%Z)%Z) /\
  (map.MapPermut.permut_sub t3 t4 l (o + 1%Z)%Z))) -> let o1 :=
  (m + 1%Z)%Z in (((0%Z <= o1)%Z /\ (r < t)%Z) -> forall (t5:(@map.Map.map
  Z _ Z _)), ((0%Z <= t)%Z /\ ((sorted_sub t5 o1 (r + 1%Z)%Z) /\
  (map.MapPermut.permut_sub t4 t5 o1 (r + 1%Z)%Z))) -> ((sorted_sub t5 l
  (r + 1%Z)%Z) /\ (map.MapPermut.permut_sub t1 t5 l (r + 1%Z)%Z)))))))).
intros n t1 l r.
intros (_, (hl, hr)) hlr hl2 v hlr2.
intros m t2 (inv1, (inv2, (inv3, (inv4, inv5)))).
intros (_, (_, hm)) t3 (_, exch).
intros o _ t4 (_, (lsorted, lpermut)).
intros o1 _ t5 (_, (rsorted, rpermut)).
subst o o1.
split.
(* sorted *)
red; intros.
assert (h: (l <= i1 < m \/ m <= i1 <= r)%Z) by omega. destruct h.
(* l <= i1 < m *)
assert (eq: Map.get t4 i1 = Map.get t5 i1).
apply permut_eq with (m+1)%Z (r+1)%Z; auto.
apply permut_sym; auto.
omega. rewrite <- eq; clear eq.
assert (vi1: (Map.get t4 i1 < v)%Z).
assert (exists i3:Z, l <= i3 < m-1+1 /\ Map.get t4 i1 = Map.get t3 i3)%Z.
apply permut_exists.
auto.
omega.
destruct H1 as (i3, (hi3, eq3)).
rewrite eq3; clear eq3.
assert (case: (i3 = l \/ l < i3)%Z) by omega. destruct case.
subst i3.
destruct exch as (_,(h,_)). rewrite h.
apply inv1; omega.
destruct exch as (_,(_,h)). rewrite <- h; try omega.
apply inv1; omega.

assert (h: (l <= i2 < m \/ m <= i2 <= r)%Z) by omega. destruct h.
(* l <= i2 < m *)
assert (eq: Map.get t4 i2 = Map.get t5 i2).
apply permut_eq with (m+1)%Z (r+1)%Z; auto.
apply permut_sym; auto.
omega.
rewrite <- eq; clear eq.
apply lsorted; omega.

(* m <= i2 <= r *)
assert (vi2: (v <= Map.get t5 i2)%Z).
assert (h: (i2 = m \/ m < i2)%Z) by omega. destruct h.
(* i2 = m *)
subst i2.
replace (Map.get t5 m) with (Map.get t3 m).
replace (Map.get t3 m) with (Map.get t2 l).
omega.
red in exch; intuition.
transitivity (Map.get t4 m).
apply permut_eq with l (m-1+1)%Z; auto.
apply permut_sym; auto.
omega.
apply permut_eq with (m+1)%Z (r+1)%Z; auto.
apply permut_sym; auto.
omega.
(* m < i2 *)
assert (exists i3:Z, m+1 <= i3 < r+1 /\ Map.get t5 i2 = Map.get t4 i3)%Z.
apply permut_exists.
auto.
omega.
destruct H3 as (i3, (hi3, eq3)).
rewrite eq3; clear eq3.
replace (Map.get t4 i3) with (Map.get t3 i3).
destruct exch as (_,(_,hex)). rewrite <- hex.
apply inv2; omega.
omega.
apply permut_eq with l (m-1+1)%Z; auto.
apply permut_sym; auto.
omega.
omega.

(* m <= i1 <= r *)
assert (vi1: (v <= Map.get t5 i1)%Z).
assert (h: (i1 = m \/ m < i1)%Z) by omega. destruct h.
(* i1 = m *)
subst i1.
replace (Map.get t5 m) with (Map.get t3 m).
replace (Map.get t3 m) with (Map.get t2 l).
omega.
red in exch; intuition.
transitivity (Map.get t4 m).
apply permut_eq with l (m-1+1)%Z; auto.
apply permut_sym; auto.
omega.
apply permut_eq with (m+1)%Z (r+1)%Z; auto.
apply permut_sym; auto.
omega.
(* m < i1 *)
assert (exists i3:Z, m+1 <= i3 < r+1 /\ Map.get t5 i1 = Map.get t4 i3)%Z.
apply permut_exists.
auto.
omega.
destruct H2 as (i3, (hi3, eq3)).
rewrite eq3; clear eq3.
replace (Map.get t4 i3) with (Map.get t3 i3).
destruct exch as (_,(_,hex)). rewrite <-hex.
apply inv2; omega.
omega.
apply permut_eq with l (m-1+1)%Z; auto.
apply permut_sym; auto.
omega.

assert (h: (l <= i2 < m \/ m <= i2 <= r)%Z) by omega. destruct h.
(* l <= i2 < m: absurd *)
omega.
(* m <= i2 <= r *)
assert (h: (i2 = m \/ m < i2)%Z) by omega. destruct h.
(* i2 = m *)
assert (eq: i1 = m) by omega.
subst; omega.
(* m < i2 *)
assert (h: (i1 = m \/ m < i1)%Z) by omega. destruct h.
(* i1 = m *)
subst i1.
assert (exists i3:Z, m+1 <= i3 < r+1 /\ Map.get t5 i2 = Map.get t4 i3)%Z.
apply permut_exists.
auto.
omega.
destruct H3 as (i3, (hi3, eq3)).
rewrite eq3; clear eq3.
replace (Map.get t4 i3) with (Map.get t3 i3).
destruct exch as (ex1,(ex2,hex)). rewrite <- hex.
replace (Map.get t5 m) with v.
apply inv2; omega.
replace (Map.get t5 m) with (Map.get t3 m).
replace (Map.get t3 m) with (Map.get t2 l).
omega.
transitivity (Map.get t4 m).
apply permut_eq with l (m-1+1)%Z; auto.
apply permut_sym; auto.
omega.
apply permut_eq with (m+1)%Z (r+1)%Z; auto.
apply permut_sym; auto.
omega.
omega.
apply permut_eq with l (m-1+1)%Z; auto.
apply permut_sym; auto.
omega.
(* m < i1 *)
apply rsorted; try omega.

(* permut *)
apply permut_trans with t4.
apply permut_trans with t3.
apply permut_trans with t2; auto.
apply permut_exchange with l m; auto.
omega.
apply permut_weakening with l (m-1+1)%Z; auto.
omega.
apply permut_weakening with (m+1)%Z (r+1)%Z; auto.
omega.
Qed.


