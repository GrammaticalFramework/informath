(* A test of scripts/rocq and RunInformath -from-formalism=rocq (make from_formalism_tests) *)
Fixpoint sum_to (n : nat) : nat :=
  match n with
  | O => O
  | S m => sum_to m + S m
  end.

Definition double (n : nat) : nat := n + n.

Theorem double_le : forall n : nat, n <= double n.
Proof.
  intros n. unfold double. induction n.
  - auto.
  - simpl. apply le_n_S. rewrite <- plus_n_Sm. auto.
Qed.

Theorem ex_bigger : forall n : nat, exists m : nat, n < m.
Proof.
  intros n. exists (S n). auto.
Qed.
