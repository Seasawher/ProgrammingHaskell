/-- アルファベットのインデックス -/
structure Index where
  /-- 番号。`0`から`25`の数字 -/
  index : Nat
  /-- 小文字かどうか -/
  isLower : Bool
deriving Inhabited, DecidableEq


/-- アルファベットを`Index`に変換する -/
def Char.toIndex (c : Char) : Index :=
  if c.isLower then
    ⟨c.val - 'a'.val |>.toNat, true⟩
  else if c.isUpper then
    ⟨c.val - 'A'.val |>.toNat, false⟩
  else
    panic! s!"toIndex: input is {c}, which is not an alphabet"

#guard 'a'.toIndex = ⟨0, true⟩
#guard 'B'.toIndex = ⟨1, false⟩


/-- インデックスをアルファベットに変換する -/
def Char.ofIndex (i : Index) : Char :=
  if i.isLower then
    Char.ofNat ('a'.val.toNat + i.index)
  else
    Char.ofNat ('A'.val.toNat + i.index)

#guard Char.ofIndex ⟨0, true⟩ = 'a'
#guard Char.ofIndex ⟨1, false⟩ = 'B'


/-- アルファベットを`n`だけシフトする -/
def Char.shift (c : Char) (n : Int) : Char :=
  if c.isAlpha then
    let code := (c.toIndex.index + n) % 26 |>.toNat
    let index := { index := code, isLower := c.isLower : Index}
    Char.ofIndex index
  else
    c

#guard Char.shift 'a' 3 = 'd'
#guard Char.shift 'z' 3 = 'c'
#guard Char.shift 'B' (-3) = 'Y'


/-- シーザー暗号の実装。文字列に登場する文字をシフトする。 -/
def String.encode (s : String) (n : Int) : String :=
  s.map (Char.shift · n)

#guard "I am a magician.".encode 3 = "L dp d pdjlfldq."
#guard "L dp d pdjlfldq.".encode (-3) = "I am a magician."
