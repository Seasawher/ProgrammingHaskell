import ProgrammingHaskell.Part1.Chapter09.Section2

/-- 途中に現れる可能性のある数式 -/
inductive Expr where
  /-- 数値リテラル -/
  | val : Pos → Expr
  /-- 演算子の適用 -/
  | app : Op → Expr → Expr → Expr

private def Expr.toString : Expr → String
  | Expr.val x => ToString.toString x
  | Expr.app op l r =>
    brak l ++ ToString.toString op ++ brak r
where
  brak : Expr → String
  | .val n => ToString.toString n
  | e => "(" ++ toString e ++ ")"

instance : ToString Expr := ⟨Expr.toString⟩

section
/- `Expr` の `ToString` インスタンスが上手くいっているかどうかテストする -/

open Op Expr

private def «1 + (2 * 3)» : Expr :=
  Expr.app add (val 1) (app mul (val 2) (val 3))

/-- info: 1+(2*3) -/
#guard_msgs in
  #eval «1 + (2 * 3)»

end

/-- 式の中に含まれる数値をリストにして返す -/
def Expr.values : Expr → List Pos
  | .val x => [x]
  | .app _ l r => l.values ++ r.values

/-- 式全体の値を評価する。途中で値が `Pos` にならなくなると、失敗する。 -/
def Expr.eval : Expr → Option Pos
  | .val x => x
  | .app op l r => do
    let x ← l.eval
    let y ← r.eval
    if h : op.valid x y then
      op.vapply x y h
    else
      none
