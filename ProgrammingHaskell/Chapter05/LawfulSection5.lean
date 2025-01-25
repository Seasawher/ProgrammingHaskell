/-- アルファベットの小文字 -/
abbrev LowerLetter := { c : Char // c.isLower }


namespace LowerLetter
  -- ## `UInt32` への変換を行う関数

  /-- 範囲証明付きの`UInt32`への変換 -/
  def toUInt32 (c : LowerLetter) : { u : UInt32 // u ≥ 97 && u ≤ 122 } :=
    ⟨Char.val c, by
      have := c.property
      simp_all [Char.isLower]
    ⟩

  /-- 範囲証明なしの`UInt32`への変換 -/
  def toUInt32! (c : LowerLetter) : UInt32 :=
    (toUInt32 c).val

end LowerLetter


namespace LowerLetter
  -- ## `UInt32` から `LowerLetter` への変換を行う関数

  /-- `UInt32`から`LowerLetter`への変換 -/
  def ofUInt32? (u : UInt32) : Option LowerLetter :=
    if h : u.isValidChar then
      let char : Char := ⟨u, h⟩
      if range : char.isLower then
        some ⟨char, range⟩
      else
        none
    else
      none

  -- instance : Coe UInt32 Nat := ⟨UInt32.toNat⟩
  -- attribute [coe] UInt32.toNat

  -- @[simp]
  -- theorem _root_.UInt32.le_to_nat (u v : UInt32) : u ≤ v ↔ u.toNat ≤ v.toNat := by
  --   exact UInt32.le_iff_toNat_le

  -- @[simp]
  -- theorem _root_.UInt32.ge_to_nat (u v : UInt32) : u ≥ v ↔ u.toNat ≥ v.toNat := by
  --   simp only [GE.ge, UInt32.le_to_nat]

  /-- `UInt32`から`LowerLetter`への範囲証明付きの変換 -/
  def ofUInt32 (u : UInt32) (hu : u ≥ 97 && u ≤ 122) : LowerLetter :=
    let char : Char := ⟨u, by
      replace hu : 97 ≤ u.toNat ∧ u.toNat ≤ 122 := by simpa using hu
      suffices goal : u.toNat < 55296 ∨ 57343 < u.toNat ∧ u.toNat < 1114112 from by
        simp_all [UInt32.isValidChar, Nat.isValidChar]
      omega
    ⟩
    ⟨char, by dsimp [Char.isLower]; exact hu⟩

end LowerLetter


-- /-- アルファベットの小文字を`n`だけシフトする -/
-- def Char.shift (c : LowerLetter) (n : Int) : Char :=
--   if c.isLower then
--     let code := (let2int c + n) % 26
--     int2let code.toNat
--   else
--     c
