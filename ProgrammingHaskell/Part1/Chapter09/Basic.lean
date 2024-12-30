/- # 9 章 カウントダウン問題 -/

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

/-- `op` を適用したときに正の整数が生成されるかどうかチェックする -/
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

/-- 途中に現れる可能性のある数式 -/
inductive Expr where
  /-- 数値リテラル -/
  | val : Pos → Expr
  /-- 演算子の適用 -/
  | app : Op → Expr → Expr → Expr
