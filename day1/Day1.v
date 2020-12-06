Require Import List.
Require Import Coq.Arith.PeanoNat.
Require Import Coq.Arith.EqNat.

Fixpoint matching (l: list nat) (it: nat) : option nat :=
  match l with
  | x :: xs => if (it + x =? 2020) then Some x else matching xs it
  | nil => None
  end.

Fixpoint matching_entries (l: list nat) : option (nat * nat) :=
  match l with
  | x :: xs =>
    match matching xs x with
    | Some y => Some (x, y)
    | None => matching_entries xs
    end
  | nil => None
  end.

Lemma matching_correct : forall it y l, matching l it = Some y -> it + y = 2020.
Proof.
  induction l; intros.
  - discriminate H.
  - simpl in *.
    destruct (it + a =? 2020) eqn:E.
    + apply Nat.eqb_eq in E. inversion H. subst. assumption.
    + auto.
Qed.

Lemma matching_complete : forall l it y,
  matching l it = None -> In y l ->
  it + y <> 2020.
Proof.
  induction l as [| x xs IH ].
  - contradiction.
  - intros it y H__matching H__y.
    simpl in *.
    destruct (it + x =? 2020) eqn:E; try discriminate H__matching.
    apply beq_nat_false in E.
    destruct H__y; subst; auto.
Qed.

Theorem solution_correct :
  forall l x1 x2, matching_entries l = Some (x1, x2) -> x1 + x2 = 2020.
Proof.
  induction l as [| x xs ]; intros.
  - discriminate H.
  - simpl in H.
    destruct (matching xs x) eqn:E.
    + inversion H. subst.
      apply matching_correct with (l:=xs).
      apply E.
    + auto.
Qed.

Theorem solution_complete : forall l,
  matching_entries l = None -> ForallOrdPairs (fun x1 x2 => x1 + x2 <> 2020) l.
Proof.
  induction l as [| x xs IHl ].
  - constructor.
  - intros.
    simpl in H. destruct (matching xs x) eqn:E; try discriminate H.
    (* matching_entries xs = None *)

    constructor; try auto.
    (* _ |- forall x2 in xs. x + x2 <> 2020 *)

    apply Forall_forall. intros x' x'_in_xs.
    eapply matching_complete with (y:=x') in E; assumption.
Qed.

Definition solution (l: list nat) :=
  option_map (fun (p:nat*nat) => let (x, y) := p in x * y) (matching_entries l).


(* Extraction *)

Require Extraction.
Extraction Language Haskell.
Require Import ExtrHaskellBasic.
Require Import ExtrHaskellNatNum.
Extract Inductive Datatypes.nat => "Prelude.Integer" ["0" "Prelude.succ"]
"(\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))".

Extraction "./Day1.hs" solution.
