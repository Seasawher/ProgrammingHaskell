/-- アルファベットの小文字 -/
abbrev LowerLetter := { c : Char // c.isLower }


namespace LowerLetter
  -- ## `UInt32` への変換を行う関数

  /-- 範囲証明付きの`UInt32`への変換 -/
  def toUInt32ₛ (c : LowerLetter) : { u : UInt32 // u ≥ 97 && u ≤ 122 } :=
    ⟨Char.val c, by
      have := c.property
      simp_all [Char.isLower]
    ⟩

  /-- 範囲証明なしの`UInt32`への変換 -/
  def toUInt32 (c : LowerLetter) : UInt32 :=
    (toUInt32ₛ c).val

end LowerLetter


namespace LowerLetter
  -- ## `UInt32` から `LowerLetter` への変換を行う関数

  private def _root_.Char.ofUInt32? (u : UInt32) : Option Char :=
    if h : u.isValidChar then some ⟨u, h⟩ else none

  private def ofChar? (c : Char) : Option LowerLetter :=
    if h : c.isLower then some ⟨c, h⟩ else none

  /-- `UInt32`から`LowerLetter`への変換 -/
  def ofUInt32? (u : UInt32) : Option LowerLetter := do
    let char ← Char.ofUInt32? u
    ofChar? char

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
