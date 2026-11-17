-- A test of scripts/lean and RunInformath -from-formalism=lean (make from_formalism_tests)
/-- The sum of the first n numbers. -/
def sumTo : Nat → Nat
  | 0 => 0
  | n + 1 => sumTo n + (n + 1)

def double (n : Nat) : Nat := n + n

theorem double_eq (n : Nat) : double n = 2 * n := by
  unfold double; omega

theorem le_double (n : Nat) (h : 0 < n) : n ≤ double n := by
  unfold double; omega

inductive Color where
  | red | green
  deriving Repr

structure Point where
  x : Nat
  y : Nat

def Point.swap (p : Point) : Point := ⟨p.y, p.x⟩

theorem exists_bigger (n : Nat) : ∃ m, n < m := ⟨n + 1, Nat.lt_succ_self n⟩

theorem and_swap' {p q : Prop} (h : p ∧ q) : q ∧ p := ⟨h.2, h.1⟩

def evenNum (n : Nat) : Prop := ∃ k, n = 2 * k

axiom myAxiom : ∀ n : Nat, evenNum (2 * n)

theorem list_len {α : Type} (xs : List α) : xs.length = xs.length := rfl
