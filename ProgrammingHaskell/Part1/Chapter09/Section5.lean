import ProgrammingHaskell.Part1.Chapter09.Section4

/-- 与えられた式 `expr : Expr` が `inputs` と `target` の定義する
カウントダウン問題の解になっているかどうか判定する -/
def Expr.solution (expr : Expr) (inputs : List Pos) (target : Pos) : Bool :=
  expr.eval == some target && inputs.choices.contains expr.values


section «`Expr.solution` 関数のテスト»

open Op Expr

private def «1 + (2 * 3)» : Expr :=
  Expr.app add (val 1) (app mul (val 2) (val 3))

-- 式 `1 + (2 * 3)` は `inputs := [1, 2, 3, 8, 13]` と
-- `target := 7` で定義される問題の解になっている
#guard «1 + (2 * 3)».solution [1, 2, 3, 8, 13] 7

private def «(25 - 10) * (1 + 50)» : Expr :=
  Expr.app mul (app sub (val 25) (val 10)) (app add (val 1) (val 50))

#guard «(25 - 10) * (1 + 50)».solution [1, 3, 7, 10, 25, 50] 765

end «`Expr.solution` 関数のテスト»
