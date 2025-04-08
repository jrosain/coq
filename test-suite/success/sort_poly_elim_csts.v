Set Universe Polymorphism.

Module Syntax.
  Definition foo@{|u| Type -> Prop} := Type@{u}.
  Definition foo'@{| Prop -> SProp} := Prop.

  Definition bar@{s|u|s -> Type} := Type@{s|u}.
  Definition bar'@{s|u|Prop -> s} := Type@{s|u}.

  Definition bar''@{s s'| |s -> s', s' -> Prop} := Prop.
  Fail Definition bar'''@{s| |s -> Type, Prop -> s} := Prop.
End Syntax.

Inductive sum@{sl sr s|ul ur|} (A : Type@{sl|ul}) (B : Type@{sr|ur}) : Type@{s|max(ul,ur)} :=
| inl : A -> sum A B
| inr : B -> sum A B.

Arguments inl {A B} _ , [A] B _.
Arguments inr {A B} _ , A [B] _.

Definition sum_elim@{sl sr s s'|ul ur v|s -> s'}
  (A : Type@{sl|ul}) (B : Type@{sr|ur}) (P : sum@{sl sr s|ul ur} A B -> Type@{s'|v})
  (fl : forall a, P (inl a)) (fr : forall b, P (inr b)) (x : sum@{sl sr s|ul ur} A B) :=
    match x with
    | inl a => fl a
    | inr b => fr b
    end.

Definition sum_sind := sum_elim@{Type Type Type SProp|_ _ _}.
Definition sum_rect := sum_elim@{Type Type Type Type|_ _ _}.
Definition sum_ind := sum_elim@{Type Type Type Prop|_ _ _}.

Definition or_ind := sum_elim@{Prop Prop Prop Prop|_ _ _}.
Definition or_sind := sum_elim@{Prop Prop Prop SProp|_ _ _}.
Fail Definition or_rect := sum_elim@{Prop Prop Prop Type|_ _ _}.

Definition sumor := sum@{Type Prop Type|_ _}.

Definition sumor_sind := sum_elim@{Type Prop Type SProp|_ _ _}.
Definition sumor_rect := sum_elim@{Type Prop Type Type|_ _ _}.
Definition sumor_ind := sum_elim@{Type Prop Type Prop|_ _ _}.

Fail Definition idT@{sl sr s|ul ur|} (A : Type@{sl|ul}) (B : Type@{sr|ur}) (x : sum@{sl sr s|ul ur} A B)
  : sum@{sl sr Type|ul ur} A B :=
  match x return sum@{sl sr Type|ul ur} A B with
  | inl a => inl a
  | inr b => inr b
  end.

Fail Definition idP@{sl sr s|ul ur|} (A : Type@{sl|ul}) (B : Type@{sr|ur}) (x : sum@{sl sr s|ul ur} A B)
  : sum@{sl sr Prop|ul ur} A B :=
  match x return sum@{sl sr Prop|ul ur} A B with
  | inl a => inl a
  | inr b => inr b
  end.

Fail Definition idS@{sl sr s|ul ur|} (A : Type@{sl|ul}) (B : Type@{sr|ur}) (x : sum@{sl sr s|ul ur} A B)
  : sum@{sl sr SProp|ul ur} A B :=
  match x return sum@{sl sr SProp|ul ur} A B with
  | inl a => inl a
  | inr b => inr b
  end.

Fail Definition idV@{sl sr s s'|ul ur|} (A : Type@{sl|ul}) (B : Type@{sr|ur}) (x : sum@{sl sr s|ul ur} A B)
  : sum@{sl sr s'|ul ur} A B :=
  match x return sum@{sl sr s'|ul ur} A B with
  | inl a => inl a
  | inr b => inr b
  end.

Definition idT@{sl sr s|ul ur|s -> Type} (A : Type@{sl|ul}) (B : Type@{sr|ur}) (x : sum@{sl sr s|ul ur} A B)
  : sum@{sl sr Type|ul ur} A B :=
  match x return sum@{sl sr Type|ul ur} A B with
  | inl a => inl a
  | inr b => inr b
  end.

Definition idP@{sl sr s|ul ur|s -> Prop} (A : Type@{sl|ul}) (B : Type@{sr|ur}) (x : sum@{sl sr s|ul ur} A B)
  : sum@{sl sr Prop|ul ur} A B :=
  match x return sum@{sl sr Prop|ul ur} A B with
  | inl a => inl a
  | inr b => inr b
  end.

Definition idS@{sl sr s|ul ur|s -> SProp} (A : Type@{sl|ul}) (B : Type@{sr|ur}) (x : sum@{sl sr s|ul ur} A B)
  : sum@{sl sr SProp|ul ur} A B :=
  match x return sum@{sl sr SProp|ul ur} A B with
  | inl a => inl a
  | inr b => inr b
  end.

Definition idV@{sl sr s|ul ur|} (A : Type@{sl|ul}) (B : Type@{sr|ur}) (x : sum@{sl sr s|ul ur} A B)
  : sum@{sl sr s|ul ur} A B :=
  match x return sum@{sl sr s|ul ur} A B with
  | inl a => inl a
  | inr b => inr b
  end.

Inductive List'@{s|l|} (A : Type@{s|l}) : Type@{s|l} :=
| nil' : List' A
| cons' : A -> List' A -> List' A.

Arguments nil' {A}.
Arguments cons' {A} _ _.

Definition list'_elim@{s s'|l l'|s -> s'}
  (A : Type@{s|l}) (P : List'@{s|l} A -> Type@{s'|l'})
  (fn : P nil') (fc : forall (x : A) (l : List' A), P l -> P (cons' x l)) :=
  fix F (l : List'@{s|l} A) : P l :=
    match l with
    | nil' => fn
    | cons' x l => fc x l (F l)
    end.

Fail Fixpoint list'_idT@{s|l|} {A : Type@{s|l}} (l : List'@{s|l} A) : List'@{Type|l} A :=
  match l with
  | nil' => nil'
  | cons' x l => cons' x (list'_idT l)
  end.

Fail Fixpoint list'_idP@{s|l|} {A : Type@{s|l}} (l : List'@{s|l} A) : List'@{Prop|l} A :=
  match l with
  | nil' => nil'
  | cons' x l => cons' x (list'_idP l)
  end.

Fail Fixpoint list'_idS@{s|l|} {A : Type@{s|l}} (l : List'@{s|l} A) : List'@{SProp|l} A :=
  match l with
  | nil' => nil'
  | cons' x l => cons' x (list'_idS l)
  end.

Fail Fixpoint list'_idV@{s s'|l l'|l <= l'} {A : Type@{s|l}} (l : List'@{s|l} A) : List'@{s'|l'} A :=
  match l with
  | nil' => nil'
  | cons' x l => cons' x (list'_idV l)
  end.

Fixpoint list'_idT@{s|l|s -> Type} {A : Type@{s|l}} (l : List'@{s|l} A) : List'@{Type|l} A :=
  match l with
  | nil' => nil'
  | cons' x l => cons' x (list'_idT l)
  end.

Fixpoint list'_idP@{s|l|s -> Prop} {A : Type@{s|l}} (l : List'@{s|l} A) : List'@{Prop|l} A :=
  match l with
  | nil' => nil'
  | cons' x l => cons' x (list'_idP l)
  end.

Fixpoint list'_idS@{s|l|s -> SProp} {A : Type@{s|l}} (l : List'@{s|l} A) : List'@{SProp|l} A :=
  match l with
  | nil' => nil'
  | cons' x l => cons' x (list'_idS l)
  end.

Fixpoint list'_idV@{s s'|l l'|l <= l', s -> s'} {A : Type@{s|l}} (l : List'@{s|l} A) : List'@{s'|l'} A :=
  match l with
  | nil' => nil'
  | cons' x l => cons' x (list'_idV l)
  end.
