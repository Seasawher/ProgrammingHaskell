import ProgrammingHaskell.Part1.Chapter09.Section5

instance : Monad List where
  pure := List.singleton
  bind := List.flatMap

/-- あるリストを（要素の順番を保ちながら）２つの空でないリストに分割するすべての方法を返す -/
def List.split {α : Type} (xs : List α) : List (List α × List α) :=
  match xs with
  | [] => []
  | [_] => []
  | x :: xs =>
    let rest : List (List α × List α) := do
      let (ls, rs) ← List.split xs
      pure (x :: ls, rs)
    ([x], xs) :: rest

#guard [1, 2].split = [([1], [2])]
#guard [1, 2, 3].split = [([1], [2, 3]), ([1, 2], [3])]

/-- `Op` をリストとして表現したもの -/
def Op.asList : List Op := [Op.add, Op.sub, Op.mul, Op.div]

/-- 与えられた数がそれぞれちょうど一回使われている式をすべて求める。
ただし、与えられた数の順序は変更しない。 -/
partial def List.exprs (nums : List Pos) : List Expr :=
  match nums with
  | [] => []
  | [num] => [Expr.val num]
  | ns => do
    let (ls, rs) ← ns.split
    let l ← exprs ls
    let r ← exprs rs
    combine l r
where
  combine (l r : Expr) : List Expr := do
    let op ← Op.asList
    return Expr.app op l r

/-- info: [1+2, 1-2, 1*2, 1/2] -/
#guard_msgs in
  #eval [1, 2].exprs

#guard [1, 2, 3].exprs.length = 32

/-- カウントダウン問題に対して、可能な解をすべて求める -/
def Expr.solutions (nums : List Pos) (target : Pos) : List Expr := do
  let nums' ← nums.choices
  let e ← nums'.exprs
  if e.eval != target then []
  return e

/-- info: [1+9, 9+1] -/
#guard_msgs in
  #eval Expr.solutions [1, 9] 10

-- インタプリンタでは重すぎて実行できない
-- #eval Expr.solutions [1, 3, 7, 10, 25, 50] 765
