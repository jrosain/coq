(** This file tests how locality attributes affect usual vernacular commands.
    PLEASE, when this file fails to compute following a voluntary change in
    Coq's behaviour, modify accordingly the tables in [sections.rst] and
    [modules.rst] in [doc/sphinx/language/core] *)

(** This structure is used to test availability or not of a
    [Canonical Structure]. *)
Structure PointedType : Type := { Carrier :> Set; point : Carrier }.

(** This HintDb is used to test availability or not of a [Hint] command. *)
Create HintDb plop.

(** ** Tests for sections and visibility attributes *)

(** *** Without attribute (default) *)

Module InSectionDefault.

Section Bar.
  (* A parameter: *)
  Parameter (secret : nat).
  (* An axiom: *)
  Axiom secret_is_42 : secret = 42.
  (* A custom tactic: *)
  Ltac find_secret := rewrite secret_is_42.
  (* An abbreviation: *)
  Notation add_42 := (Nat.add 42).
  (* A tactic notation: *)
  Tactic Notation "rfl" := reflexivity.
  (* A notation: *)
  Infix "+p" := Nat.add (only parsing, at level 30, right associativity) : nat_scope.
  (* A lemma: *)
  Lemma secret_42 : secret = 42.
  Proof. find_secret. rfl. Qed.
  (* A Canonical Structure: *)
  Canonical natPointed : PointedType := {| Carrier := nat; point := 42 |}.
  (* A Coercion: *)
  Coercion to_nat (b : bool) := if b then 1 else 0.
  (* A Setting: *)
  Set Universe Polymorphism.
  (* A Hint: *)
  Hint Resolve secret_42 : plop.
End Bar.

(* Availability of the parameter *)
Check secret.
(* Availability of the axiom *)
Check secret_is_42.
(* Availability of the tactic *)
Fail Print find_secret.
(* Availability of the abbreviation *)
Fail Check add_42.
(* Availability of the tactic notation *)
Lemma plop_i : 2 + 2 = 4.
Proof. Fail rfl. Admitted.
(* Availability of the notation *)
Fail Check (2 +p 3).
(* Availability of the canonical structure *)
Check (point nat).
(* Availability of the coercion *)
Check (true + 2).
(* Availability of [Set Universe Polymorphism] *)
Definition foo_i@{u} := nat.
Check foo_i@{_}.
(* Availability of the [Hint] *)
Lemma hop_i : secret = 42.
Proof.
  Fail solve [auto with plop].
Admitted.

End InSectionDefault.

Module InSectionLocal.

Section Bar.
  (* A parameter: *)
  #[local]
  Parameter (secret : nat).
  (* An axiom: *)
  #[local]
  Axiom secret_is_42 : secret = 42.
  (* A custom tactic: *)
  #[local]
  Ltac find_secret := rewrite secret_is_42.
  (* An abbreviation: *)
  #[local]
  Notation add_42 := (Nat.add 42).
  (* A tactic notation: *)
  #[local]
  Tactic Notation "rfl" := reflexivity.
  (* A notation: *)
  #[local]
  Infix "+p" := Nat.add (only parsing, at level 30, right associativity) : nat_scope.
  (* A lemma: *)
  #[local]
  Lemma secret_42 : secret = 42.
  Proof. find_secret. rfl. Qed.
  (* A Canonical Structure *)
  #[local]
  Canonical natPointed : PointedType := {| Carrier := nat; point := 42 |}.
  (* A Coercion *)
  #[local]
  Coercion to_nat (b : bool) := if b then 1 else 0.
  (* A Setting *)
  #[local]
  Set Universe Polymorphism.
  (* A Hint *)
  #[local]
  Hint Resolve secret_42 : plop.
End Bar.

(** **** Without importing: *)

(* Availability of the parameter *)
Check secret.
(* Availability of the axiom *)
Check secret_is_42.
(* Availability of the tactic *)
Fail Print find_secret.
(* Availability of the abbreviation *)
Fail Check add_42.
(* Availability of the tactic notation *)
Lemma plop_ni : 2 + 2 = 4.
Proof. Fail rfl. Admitted.
(* Availability of the notation *)
Fail Check (2 +p 3).
(* Availability of the canonical structure *)
Fail Check (point nat).
(* Availability of the coercion *)
Fail Check (true + 2).
(* Availability of [Set Universe Polymorphism] *)
Definition foo_ni@{u} := nat.
Fail Check foo_ni@{_}.
(* Availability of the [Hint] *)
Lemma hop_ni : secret = 42.
Proof.
  Fail solve [auto with plop].
Admitted.
End InSectionLocal.

Module InSectionExport.

Section Bar.
  (* A parameter: *)
  Fail #[export]
  Parameter (secret : nat).
  (* An axiom: *)
  Fail #[export]
  Axiom plop : 0 = 0.
  (* A custom tactic: *)
  Fail #[export]
  Ltac find_secret := reflexivity.
  (* An abbreviation: *)
  Fail #[export]
  Notation add_42 := (Nat.add 42).
  (* A tactic notation: *)
  Fail #[export]
  Tactic Notation "rfl" := reflexivity.
  (* A notation: *)
  Fail #[export]
  Infix "+p" := Nat.add (only parsing, at level 30, right associativity) : nat_scope.
  (* A lemma: *)
  Fail #[export]
  Lemma secret_42 : secret = 42.
  (* A Canonical Structure *)
  Fail #[export]
  Canonical natPointed : PointedType := {| Carrier := nat; point := 42 |}.
  (* A Coercion *)
  Fail#[export]
  Coercion to_nat (b : bool) := if b then 1 else 0.
  (* A Setting *)
  #[export]
  Set Universe Polymorphism.
  (* A Hint *)
  Parameter (secret : nat).
  Axiom secret_42 : secret = 42.
  Fail #[export]
  Hint Resolve secret_42 : plop.
End Bar.

(* Availability of [Set Universe Polymorphism] *)
Definition foo_ni@{u} := nat.
Check foo_ni@{_}.

End InSectionExport.

Module InSectionGlobal.
Section Bar.
  (* A parameter: *)
  #[global]
  Parameter (secret : nat).
  (* An axiom: *)
  #[global]
  Axiom secret_is_42 : secret = 42.
  (* A custom tactic: *)
  Fail #[global]
  Ltac find_secret := rewrite secret_is_42.
  (* An abbreviation: *)
  Fail #[global]
  Notation add_42 := (Nat.add 42).
  (* A tactic notation: *)
  Fail #[global]
  Tactic Notation "rfl" := reflexivity.
  (* A notation: *)
  Fail #[global]
  Infix "+p" := Nat.add (only parsing, at level 30, right associativity) : nat_scope.
  (* A Canonical Structure *)
  #[global]
  Canonical natPointed : PointedType := {| Carrier := nat; point := 42 |}.
  (* A Coercion *)
  #[global]
  Coercion to_nat (b : bool) := if b then 1 else 0.
  (* A Setting *)
  #[global]
  Set Universe Polymorphism.
  (* A Hint *)
  Fail #[global]
  Hint Resolve secret_is_42 : plop.
End Bar.

(** **** Without importing: *)

(* Availability of the parameter *)
Check secret.
(* Availability of the axiom *)
Check secret_is_42.
(* Availability of the canonical structure *)
Check (point nat).
(* Availability of the coercion *)
Check (true + 2).
(* Availability of [Set Universe Polymorphism] *)
Definition foo_ni@{u} := nat.
Check foo_ni@{_}.

End InSectionGlobal.
