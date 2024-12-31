/-- 許されている演算の集合 -/
inductive Op where
  /-- 加法 -/
  | add
  /-- 減法 -/
  | sub
  /-- 乗法 -/
  | mul
  /-- 除法 -/
  | div

private def Op.toString : Op → String
  | Op.add => "+"
  | Op.sub => "-"
  | Op.mul => "*"
  | Op.div => "/"

instance : ToString Op := ⟨Op.toString⟩

/-- 正の自然数。途中の状態として許可されるのは正の自然数のみ。 -/
abbrev Pos := { n : Nat // n > 0 }

private def Pos.ofNat (n : Nat) : Pos :=
  if h : n = 0 then ⟨1, by decide⟩
  else ⟨n, by omega⟩

instance {n : Nat} : OfNat Pos (n + 1) := ⟨Pos.ofNat (n + 1)⟩

instance : ToString Pos := inferInstance

/-- `op` を適用したときに正の整数が生成されるかどうかチェックする

**Note** 本では引数の型は `Int` になっているが、文脈からして型は `Pos` であるべきだと考えられる。
-/
def Op.valid (op : Op) (x y : Pos) : Bool :=
  match op with
  | Op.add => true
  | Op.sub => x.val > y.val
  | Op.mul => true
  | Op.div => x.val % y.val == 0

/-- 有効な演算子の適用を実行する -/
def Op.apply (op : Op) (x y : Pos) : Nat :=
  match op with
  | Op.add => x.val + y.val
  | Op.sub => x.val - y.val
  | Op.mul => x.val * y.val
  | Op.div => x.val / y.val

/-- `op.valid x y` が成立しているならば、`op.apply x y` は正の数 -/
theorem Op.pos_of_valid (op : Op) (x y : Pos) (h : op.valid x y) : op.apply x y > 0 := by
  dsimp [Op.apply]
  cases op
  case add =>
    dsimp
    omega
  case mul =>
    dsimp
    have xpos : x.val > 0 := x.property
    have ypos : y.val > 0 := y.property
    exact Nat.mul_pos xpos ypos
  case sub =>
    dsimp [Op.valid] at h ⊢
    have : x.val > y.val := by simp_all
    omega
  case div =>
    dsimp [Op.valid] at h ⊢
    have div : x.val % y.val = 0 := by simp_all
    replace div : y.val ∣ x.val := by exact Nat.dvd_of_mod_eq_zero div
    have xpos : x.val > 0 := x.property
    have : x.val = y.val * (x.val / y.val) := by exact Eq.symm (Nat.mul_div_cancel' div)
    suffices hyp : x.val / y.val ≠ 0 from by
      exact Nat.zero_lt_of_ne_zero hyp
    intro hyp
    simp [hyp] at this
    omega

/-- `Op.apply` の返り値が `Pos` になっているバージョン -/
def Op.vapply (op : Op) (x y : Pos) (h : op.valid x y) : Pos :=
  ⟨op.apply x y, Op.pos_of_valid op x y h⟩
