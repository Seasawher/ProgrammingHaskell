/- # 小町算をLeanで解こう

Chapter 9 の内容が楽しかったので、派生問題として小町算を解きたい
-/

abbrev α := Nat

/-- 小町算の問題 -/
structure Komachi : Type where
  /-- 数字の並び -/
  nums : List α

  /-- 目標の数 -/
  target : α

/-- 許されている2項演算の集合 -/
inductive Op where
  /-- 加法 -/
  | add
  /-- 減法 -/
  | sub
  /-- 乗法 -/
  | mul
  /-- 除法 -/
  | div
  /-- 空白。10進法に基づいて桁上がりさせる -/
  | space

private def Op.toString : Op → String
  | Op.add => "+"
  | Op.sub => "-"
  | Op.mul => "*"
  | Op.div => "/"
  | Op.space => ""

instance : ToString Op := ⟨Op.toString⟩

/-- 10進対数 -/
def Nat.log10 (n : Nat) : Nat :=
  let rec loop (n : Nat) (r : Nat) : Nat :=
    if n < 10 then r else loop (n / 10) (r + 1)
  loop n 0

/-- 単に連結して、10進法数字として解釈する -/
def Nat.append (x y : Nat) : Nat :=
  x * (10 ^ (Nat.log10 y + 1)) + y

instance : Append Nat := ⟨Nat.append⟩

/-- 演算子を適用する -/
def Op.apply (op : Op) (x y : α) : α :=
  match op with
  | Op.add => x + y
  | Op.sub => x - y
  | Op.mul => x * y
  | Op.div => x / y
  | Op.space => x ++ y

/-- 演算子の適用が正しいか判定する -/
def Op.valid (op : Op) (x y : α) : Bool :=
  match op with
  | Op.div => x % y == 0 -- 割り切れていることを要求する
  | _ => true

/-- 演算子を適用した結果得られる数式
**Note** 空白以外の演算子を適用すると、もう空白は適用できないことに注意。
このようなバリデーションルールは後で実装する。
-/
inductive Expr where
  /-- 数値リテラル -/
  | val : Int → Expr
  /-- 演算子の適用 -/
  | app : Op → Expr → Expr → Expr

--
