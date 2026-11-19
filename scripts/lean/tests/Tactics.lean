-- A test of the tactic proofs of scripts/lean: Lean core only (Nat, List, logic).
--   RunInformath -from-formalism=lean scripts/lean/tests/Tactics.lean -proof-text

def double (n : Nat) : Nat := n + n

theorem double_eq (n : Nat) : double n = 2 * n := by
  unfold double
  omega

theorem and_comm' (p q : Prop) : p ∧ q → q ∧ p := by
  intro h
  obtain ⟨hp, hq⟩ := h
  constructor
  · exact hq
  · exact hp

theorem or_comm' (p q : Prop) : p ∨ q → q ∨ p := by
  intro h
  cases h with
  | inl hp => exact Or.inr hp
  | inr hq => exact Or.inl hq

theorem imp_trans' (p q r : Prop) (hpq : p → q) (hqr : q → r) : p → r := by
  intro hp
  apply hqr
  apply hpq
  exact hp

theorem zero_add' (n : Nat) : 0 + n = n := by
  induction n with
  | zero => rfl
  | succ k ih => rw [Nat.add_succ, ih]

theorem double_le (n m : Nat) (h : n ≤ m) : double n ≤ double m := by
  have h2 : n + n ≤ m + m := by omega
  unfold double
  exact h2

theorem exists_double (n : Nat) : ∃ m, m = double n := by
  exact ⟨n + n, rfl⟩

theorem length_append' (xs ys : List Nat) : (xs ++ ys).length = xs.length + ys.length := by
  simp

theorem exists_pos (n : Nat) (h : ∃ k, n = k + 1) : 0 < n := by
  rcases h with ⟨k, hk⟩
  rw [hk]
  omega

theorem double_eq_add (n : Nat) : double n = n + n := by
  rw [double]

theorem le_double (n : Nat) : n ≤ double n := by
  have h := double_eq n
  rw [h]
  omega

theorem pos_and (n : Nat) (h : 0 < n) : 0 < n ∧ 0 < n + n := by
  constructor <;> omega

theorem lt_of_add_zero (n : Nat) (h : n + 0 < 5) : n < 5 := by
  simp at h
  exact h

theorem em' (p : Prop) : p ∨ ¬p := by
  by_cases hp : p
  · left
    exact hp
  · right
    exact hp

namespace Even

def even (n : Nat) : Prop := ∃ k, n = 2 * k

theorem even_double (n : Nat) : even (double n) := by
  refine ⟨n, ?_⟩
  exact double_eq n

theorem even_add (m n : Nat) (hm : even m) (hn : even n) : even (m + n) := by
  obtain ⟨a, ha⟩ := hm
  obtain ⟨b, hb⟩ := hn
  exists a + b
  rw [ha, hb]
  omega

theorem even_double_double (n : Nat) : even (double n + double n) := by
  apply even_add
  · apply even_double
  · exact even_double n

end Even
