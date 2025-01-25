import Batteries.Data.String.Basic

/-- アルファベットの小文字 -/
abbrev LowerLetter := { c : Char // c.isLower }

/-- アルファベットの小文字 -/
def LowerLetter.asArray : Array Char :=
  #['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
  'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
  'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ]

instance : Coe UInt32 Nat where
  coe n := n.toNat

/-- アルファベットの小文字を`0`から`25`の自然数に変換する -/
def let2int (c : Char) : Nat :=
  c.val - 'a'.val

#guard let2int 'a' = 0
#guard let2int 'z' = 25

/-- `0`から`25`の自然数をアルファベット小文字に変換する -/
def int2let (n : Nat) : Char :=
  Char.ofNat ('a'.val + n)

#guard int2let 0 = 'a'
#guard int2let 25 = 'z'

/-- アルファベットの小文字を`n`だけシフトする -/
def Char.shift (c : Char) (n : Int) : Char :=
  if c.isLower then
    let code := (let2int c + n) % 26
    int2let code.toNat
  else
    c

#guard Char.shift 'a' 3 = 'd'
#guard Char.shift 'z' 3 = 'c'
#guard Char.shift 'c' (-3) = 'z'
#guard Char.shift ' ' 3 = ' '

/-- シーザー暗号の実装。文字列に登場する文字をシフトする。 -/
def String.encode (s : String) (n : Int) : String :=
  s.map (fun c => c.shift n)

#guard "haskell is fun".encode 3 = "kdvnhoo lv ixq"
#guard "kdvnhoo lv ixq".encode (-3) = "haskell is fun"

/-- アルファベットの出現頻度表 -/
def table :=
  #[8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
  0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
  6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

#guard table.foldl (· + ·) 0 == 100.0
#guard table.size = 26

/-- 文字の出現頻度を返す関数 -/
def Char.freq (c : Char) : Float :=
  if c.isLower then
    table.get! (let2int c)
  else
    0.0

/-- 文字列の中の各アルファベットの出現頻度を調べる -/
def String.freqs (s : String) : Array Float :=
  let n := s.length.toFloat
  LowerLetter.asArray |>.map (fun c =>
    let count := (s.count c).toFloat
    count / n
  )

#guard "abbcccddddeeeee".freqs[0] < 0.067

