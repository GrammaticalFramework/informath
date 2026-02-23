Require Import Arith.

Definition false : Prop := False.
Definition and (A : Prop)(B : Prop) : Prop := A /\ B.
Definition or (A : Prop)(B : Prop) : Prop := A \/ B.
Definition If (A B : Prop) : Prop := A -> B.
Definition All (A : Type)(B : A -> Prop) : Prop := forall x : A, (B x). 
Definition Exist (A : Type)(B : A -> Prop) : Prop := exists x : A, (B x).
Definition Not (A : Prop) : Prop := ~ A.
Definition iff (A B : Prop) : Prop := (A <-> B).


Definition Num : Set := nat.

Definition Dig : Set := Num.
Definition Nat : Set := Num.
Definition Int : Set := Num.
Definition Rat : Set := Num.
Definition Real : Set := Num.
Definition Complex : Set := Num.

     
Definition Eq (x : Num) (y : Num) : Prop := x = y.
Definition Lt (x : Num) (y : Num) : Prop := x < y.
Definition Gt (x : Num) (y : Num) : Prop := x > y.

Definition Neq (x : Num) (y : Num) : Prop := not (Eq x y). 
Definition Leq (x : Num) (y : Num) : Prop := not (Gt x y).
Definition Geq (x : Num) (y : Num) : Prop := not (Lt x y).

Definition positive (x : Num) : Prop := x > 0.
Definition negative (x : Num) : Prop := x < 0.

Definition plus (x y : Num) : Num := x + y.
Definition minus (x y : Num) : Num := x - y.
Definition times (x y : Num) : Num := x * y.
Definition div (x y : Num) : Num := x / y.
			   
Definition pow (x y : Nat) : Nat := x ^ y.

(*	     
Axiom gcd (x y : Int) : Int.
Axiom factorial (x : Nat) : Nat.
*)
				     
Definition even (n : Int) : Prop := Exist Int (fun m => Eq n (times 2 m)).
Definition odd (n : Int) : Prop := not (even n).

Definition divisible (n m : Int) : Prop := Exist Int (fun k => Eq n (times k m)).

Definition prime (n : Nat) : Prop := not (Exist Nat (fun m => and (Lt 1 m) (and (Lt m n) (divisible n  m)))).

(*
(; set operations ;)
function : Set -> Set -> Set.
union : Set -> Set -> Set.
intersection : Set -> Set -> Set.
difference : Set -> Set -> Set.
powerset : Set -> Set.

(; hard-coded in MathCore ;)
suchthat : (A : Set) -> (B : Elem A -> Prop) -> Set.
fst : (A : Set) -> (B : (Elem A -> Prop)) -> Elem (suchthat A B) -> Elem A.
*)
