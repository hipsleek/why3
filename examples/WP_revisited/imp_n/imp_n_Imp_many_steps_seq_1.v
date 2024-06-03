(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require HighOrd.
Require int.Int.
Require map.Map.
Require map.Const.

Axiom ident : Type.
Parameter ident_WhyType : WhyType ident.
Existing Instance ident_WhyType.

Axiom ident_eq_dec : forall (i1:ident) (i2:ident), (i1 = i2) \/ ~ (i1 = i2).

Parameter mk_ident: Numbers.BinNums.Z -> ident.

Axiom mk_ident_inj :
  forall (i:Numbers.BinNums.Z) (j:Numbers.BinNums.Z),
  ((mk_ident i) = (mk_ident j)) -> (i = j).

(* Why3 assumption *)
Inductive operator :=
  | Oplus : operator
  | Ominus : operator
  | Omult : operator.
Axiom operator_WhyType : WhyType operator.
Existing Instance operator_WhyType.

(* Why3 assumption *)
Inductive expr :=
  | Econst : Numbers.BinNums.Z -> expr
  | Evar : ident -> expr
  | Ebin : expr -> operator -> expr -> expr.
Axiom expr_WhyType : WhyType expr.
Existing Instance expr_WhyType.

(* Why3 assumption *)
Inductive stmt :=
  | Sskip : stmt
  | Sassign : ident -> expr -> stmt
  | Sseq : stmt -> stmt -> stmt
  | Sif : expr -> stmt -> stmt -> stmt
  | Swhile : expr -> stmt -> stmt.
Axiom stmt_WhyType : WhyType stmt.
Existing Instance stmt_WhyType.

Axiom check_skip : forall (s:stmt), (s = Sskip) \/ ~ (s = Sskip).

(* Why3 assumption *)
Definition state := ident -> Numbers.BinNums.Z.

(* Why3 assumption *)
Definition eval_bin (x:Numbers.BinNums.Z) (op:operator)
    (y:Numbers.BinNums.Z) : Numbers.BinNums.Z :=
  match op with
  | Oplus => (x + y)%Z
  | Ominus => (x - y)%Z
  | Omult => (x * y)%Z
  end.

(* Why3 assumption *)
Fixpoint eval_expr (s:ident -> Numbers.BinNums.Z)
  (e:expr) {struct e}: Numbers.BinNums.Z :=
  match e with
  | Econst n => n
  | Evar x => s x
  | Ebin e1 op e2 => eval_bin (eval_expr s e1) op (eval_expr s e2)
  end.

(* Why3 assumption *)
Inductive one_step: (ident -> Numbers.BinNums.Z) -> stmt ->
  (ident -> Numbers.BinNums.Z) -> stmt -> Prop :=
  | one_step_assign :
      forall (s:ident -> Numbers.BinNums.Z) (x:ident) (e:expr),
      one_step s (Sassign x e) (map.Map.set s x (eval_expr s e)) Sskip
  | one_step_seq :
      forall (s:ident -> Numbers.BinNums.Z) (s':ident -> Numbers.BinNums.Z)
        (i1:stmt) (i1':stmt) (i2:stmt),
      one_step s i1 s' i1' -> one_step s (Sseq i1 i2) s' (Sseq i1' i2)
  | one_step_seq_skip :
      forall (s:ident -> Numbers.BinNums.Z) (i:stmt),
      one_step s (Sseq Sskip i) s i
  | one_step_if_true :
      forall (s:ident -> Numbers.BinNums.Z) (e:expr) (i1:stmt) (i2:stmt),
      ~ ((eval_expr s e) = 0%Z) -> one_step s (Sif e i1 i2) s i1
  | one_step_if_false :
      forall (s:ident -> Numbers.BinNums.Z) (e:expr) (i1:stmt) (i2:stmt),
      ((eval_expr s e) = 0%Z) -> one_step s (Sif e i1 i2) s i2
  | one_step_while_true :
      forall (s:ident -> Numbers.BinNums.Z) (e:expr) (i:stmt),
      ~ ((eval_expr s e) = 0%Z) ->
      one_step s (Swhile e i) s (Sseq i (Swhile e i))
  | one_step_while_false :
      forall (s:ident -> Numbers.BinNums.Z) (e:expr) (i:stmt),
      ((eval_expr s e) = 0%Z) -> one_step s (Swhile e i) s Sskip.

Axiom progress :
  forall (s:ident -> Numbers.BinNums.Z) (i:stmt), ~ (i = Sskip) ->
  exists s':ident -> Numbers.BinNums.Z, exists i':stmt, one_step s i s' i'.

(* Why3 assumption *)
Inductive many_steps: (ident -> Numbers.BinNums.Z) -> stmt ->
  (ident -> Numbers.BinNums.Z) -> stmt -> Numbers.BinNums.Z -> Prop :=
  | many_steps_refl :
      forall (s:ident -> Numbers.BinNums.Z) (i:stmt), many_steps s i s i 0%Z
  | many_steps_trans :
      forall (s1:ident -> Numbers.BinNums.Z) (s2:ident -> Numbers.BinNums.Z)
        (s3:ident -> Numbers.BinNums.Z) (i1:stmt) (i2:stmt) (i3:stmt)
        (n:Numbers.BinNums.Z),
      one_step s1 i1 s2 i2 -> many_steps s2 i2 s3 i3 n ->
      many_steps s1 i1 s3 i3 (n + 1%Z)%Z.

Axiom steps_non_neg :
  forall (s1:ident -> Numbers.BinNums.Z) (s2:ident -> Numbers.BinNums.Z)
    (i1:stmt) (i2:stmt) (n:Numbers.BinNums.Z),
  many_steps s1 i1 s2 i2 n -> (0%Z <= n)%Z.


(* Why3 goal *)
Theorem many_steps_seq :
  forall (s1:ident -> Numbers.BinNums.Z) (s3:ident -> Numbers.BinNums.Z)
    (i1:stmt) (i2:stmt) (n:Numbers.BinNums.Z),
  many_steps s1 (Sseq i1 i2) s3 Sskip n ->
  exists s2:ident -> Numbers.BinNums.Z, exists n1:Numbers.BinNums.Z,
  exists n2:Numbers.BinNums.Z,
  many_steps s1 i1 s2 Sskip n1 /\
  many_steps s2 i2 s3 Sskip n2 /\ (n = ((1%Z + n1)%Z + n2)%Z).
(* Why3 intros s1 s3 i1 i2 n h1. *)
Proof.
intros s1 s3 i1 i2 n Hred.
generalize Hred.
generalize (steps_non_neg _ _ _ _ _ Hred).
clear Hred.
intros H.
generalize s1 i1; clear s1 i1.
apply Z_lt_induction
 with (P := fun n =>
  forall (s1 : Map.map ident Z) (i1 : stmt),
  many_steps s1 (Sseq i1 i2) s3 Sskip n ->
exists s2 : Map.map ident Z,
  exists n1 : Z,
    exists n2 : Z,
      many_steps s1 i1 s2 Sskip n1 /\
      many_steps s2 i2 s3 Sskip n2 /\ n = (1 + n1 + n2)%Z
); auto.
intros.
inversion H1; subst; clear H1.
inversion H2; subst; clear H2.
(* case i1 <> Sskip *)
assert (h:(0 <= n0 < n0+1)%Z).
  generalize (steps_non_neg _ _ _ _ _ H3); auto with zarith.
generalize (H0 n0 h _ _ H3).
intros (s4,(n4,(n5,(h1,(h2,h3))))).
exists s4.
exists (n4+1)%Z.
exists n5.
split.
apply many_steps_trans with (1:=H8); auto.
split; auto with zarith.

(* case i1 = Sskip *)
exists s2.
exists 0%Z.
exists n0.
split.
constructor.
split; auto with zarith.
Qed.

