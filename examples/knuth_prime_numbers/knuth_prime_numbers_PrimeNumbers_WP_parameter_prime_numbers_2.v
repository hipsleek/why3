(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require HighOrd.
Require int.Int.
Require int.Abs.
Require int.EuclideanDivision.
Require int.ComputerDivision.
Require map.Map.
Require number.Parity.
Require number.Divisibility.
Require number.Prime.

(* Why3 assumption *)
Inductive ref (a:Type) :=
  | ref'mk : a -> ref a.
Axiom ref_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (ref a).
Existing Instance ref_WhyType.
Arguments ref'mk {a}.

(* Why3 assumption *)
Definition contents {a:Type} {a_WT:WhyType a} (v:ref a) : a :=
  match v with
  | ref'mk x => x
  end.

(* Why3 assumption *)
Definition no_prime_in (l:Numbers.BinNums.Z) (u:Numbers.BinNums.Z) : Prop :=
  forall (x:Numbers.BinNums.Z), (l < x)%Z /\ (x < u)%Z ->
  ~ number.Prime.prime x.

(* Why3 assumption *)
Definition first_primes (p:Numbers.BinNums.Z -> Numbers.BinNums.Z)
    (u:Numbers.BinNums.Z) : Prop :=
  ((p 0%Z) = 2%Z) /\
  (forall (i:Numbers.BinNums.Z) (j:Numbers.BinNums.Z),
   (0%Z <= i)%Z /\ (i < j)%Z /\ (j < u)%Z -> ((p i) < (p j))%Z) /\
  (forall (i:Numbers.BinNums.Z), (0%Z <= i)%Z /\ (i < u)%Z ->
   number.Prime.prime (p i)) /\
  (forall (i:Numbers.BinNums.Z), (0%Z <= i)%Z /\ (i < (u - 1%Z)%Z)%Z ->
   no_prime_in (p i) (p (i + 1%Z)%Z)).

Axiom exists_prime :
  forall (p:Numbers.BinNums.Z -> Numbers.BinNums.Z) (u:Numbers.BinNums.Z),
  (1%Z <= u)%Z -> first_primes p u -> forall (d:Numbers.BinNums.Z),
  (2%Z <= d)%Z /\ (d <= (p (u - 1%Z)%Z))%Z -> number.Prime.prime d ->
  exists i:Numbers.BinNums.Z, ((0%Z <= i)%Z /\ (i < u)%Z) /\ (d = (p i)).

Axiom Bertrand_postulate :
  forall (p:Numbers.BinNums.Z), number.Prime.prime p ->
  ~ no_prime_in p (2%Z * p)%Z.

Axiom array : forall (a:Type), Type.
Parameter array_WhyType :
  forall (a:Type) {a_WT:WhyType a}, WhyType (array a).
Existing Instance array_WhyType.

Parameter elts:
  forall {a:Type} {a_WT:WhyType a}, array a -> Numbers.BinNums.Z -> a.

Parameter length:
  forall {a:Type} {a_WT:WhyType a}, array a -> Numbers.BinNums.Z.

Axiom array'invariant :
  forall {a:Type} {a_WT:WhyType a},
  forall (self:array a), (0%Z <= (length self))%Z.

(* Why3 assumption *)
Definition mixfix_lbrb {a:Type} {a_WT:WhyType a} (a1:array a)
    (i:Numbers.BinNums.Z) : a :=
  elts a1 i.

Parameter mixfix_lblsmnrb:
  forall {a:Type} {a_WT:WhyType a}, array a -> Numbers.BinNums.Z -> a ->
  array a.

Axiom mixfix_lblsmnrb'spec :
  forall {a:Type} {a_WT:WhyType a},
  forall (a1:array a) (i:Numbers.BinNums.Z) (v:a),
  ((length (mixfix_lblsmnrb a1 i v)) = (length a1)) /\
  ((elts (mixfix_lblsmnrb a1 i v)) = (map.Map.set (elts a1) i v)).

Parameter make:
  forall {a:Type} {a_WT:WhyType a}, Numbers.BinNums.Z -> a -> array a.

Axiom make_spec :
  forall {a:Type} {a_WT:WhyType a},
  forall (n:Numbers.BinNums.Z) (v:a), (0%Z <= n)%Z ->
  (forall (i:Numbers.BinNums.Z), (0%Z <= i)%Z /\ (i < n)%Z ->
   ((mixfix_lbrb (make n v) i) = v)) /\
  ((length (make n v)) = n).

Parameter if_term: Numbers.BinNums.Z -> Init.Datatypes.bool.

Axiom if_term'def :
  forall (o:Numbers.BinNums.Z),
  ((o = 0%Z) -> ((if_term o) = Init.Datatypes.true)) /\
  (~ (o = 0%Z) -> ((if_term o) = Init.Datatypes.false)).

Require Import Lia.

(* Why3 goal *)
Theorem prime_numbers'vc :
  forall (m:Numbers.BinNums.Z), (2%Z <= m)%Z ->
  forall (p:array Numbers.BinNums.Z),
  (forall (i:Numbers.BinNums.Z), (0%Z <= i)%Z /\ (i < m)%Z ->
   ((mixfix_lbrb p i) = 0%Z)) /\
  ((length p) = m) -> forall (p1:array Numbers.BinNums.Z),
  ((length p1) = (length p)) ->
  ((elts p1) = (map.Map.set (elts p) 0%Z 2%Z)) /\
  (p1 = (mixfix_lblsmnrb p 0%Z 2%Z)) -> forall (p2:array Numbers.BinNums.Z),
  ((length p2) = (length p1)) ->
  ((elts p2) = (map.Map.set (elts p1) 1%Z 3%Z)) /\
  (p2 = (mixfix_lblsmnrb p1 1%Z 3%Z)) ->
  let o := (m - 1%Z)%Z in
  (2%Z <= (o + 1%Z)%Z)%Z ->
  forall (n:Numbers.BinNums.Z) (p3:array Numbers.BinNums.Z),
  ((length p3) = (length p2)) -> forall (j:Numbers.BinNums.Z),
  ((2%Z <= j)%Z /\ (j <= o)%Z) /\
  first_primes (elts p3) j /\
  (((mixfix_lbrb p3 (j - 1%Z)%Z) < n)%Z /\
   (n < (2%Z * (mixfix_lbrb p3 (j - 1%Z)%Z))%Z)%Z) /\
  number.Parity.odd n /\ no_prime_in (mixfix_lbrb p3 (j - 1%Z)%Z) n ->
  forall (n1:Numbers.BinNums.Z) (p4:array Numbers.BinNums.Z),
  ((length p4) = (length p3)) -> forall (k:Numbers.BinNums.Z),
  ((1%Z <= k)%Z /\ (k < j)%Z) /\
  first_primes (elts p4) j /\
  (((mixfix_lbrb p4 (j - 1%Z)%Z) < n1)%Z /\
   (n1 < (2%Z * (mixfix_lbrb p4 (j - 1%Z)%Z))%Z)%Z) /\
  number.Parity.odd n1 /\
  no_prime_in (mixfix_lbrb p4 (j - 1%Z)%Z) n1 /\
  (forall (i:Numbers.BinNums.Z), (0%Z <= i)%Z /\ (i < k)%Z ->
   ~ number.Divisibility.divides (mixfix_lbrb p4 i) n1) ->
  ((ZArith.BinInt.Z.rem n1 (mixfix_lbrb p4 k)) = 0%Z) ->
  ~ number.Prime.prime n1.
(* Why3 intros m h1 p (h2,h3) p1 h4 (h5,h6) p2 h7 (h8,h9) o h10 n p3 h11 j
        ((h12,h13),(h14,((h15,h16),(h17,h18)))) n1 p4 h19 k
        ((h20,h21),(h22,((h23,h24),(h25,(h26,h27))))) h28. *)
Proof.
intros m h1 p (h2,h3) p1 h4 h5 p2 h6 h7 o h8 n p3 h9 j
((h10,h11),(h12,((h13,h14),(h15,h16)))) n1 p4 h17 k
((h18,h19),(h20,((h21,h22),(h23,(h24,h25))))) h26.
intro h0.
red in h0. destruct h0.
red in h20. decompose [and] h20. clear h20.
apply H0 with (elts p4 k).
assert (2 < elts p4 k)%Z.
rewrite <- H1.
apply H3; lia.
split. lia.
assert (case: (k<j-1 \/ k=j-1)%Z) by lia. destruct case.
unfold mixfix_lbrb in *.
apply Z.lt_trans with (elts p4 (j-1))%Z; try lia.
apply H3; lia.
subst k; auto.
apply Divisibility.mod_divides_computer; auto.
assert (2 < elts p4 k)%Z.
rewrite <- H1.
apply H3; lia.
lia.
Qed.
