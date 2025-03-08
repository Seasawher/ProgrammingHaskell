import ProgrammingHaskell.Chapter13.Section5

/-- 述語 `p` を満たす１文字用のパーサ -/
def Parser.sat (p : Char → Bool) : Parser Char := do
  let x ← item
  guard <| p x
  return x

open Parser

#guard sat (fun x => x = 'a') "abc" = some ('a', "bc")

/-- 数字用のパーサ -/
def Parser.digit : Parser Char := sat Char.isDigit

#guard digit "123" = some ('1', "23")
#guard digit "abc" = none

/-- アルファベット小文字用のパーサ -/
def Parser.lower : Parser Char := sat Char.isLower

#guard lower "abc" = some ('a', "bc")
#guard lower "ABC" = none

/-- アルファベット大文字用のパーサ -/
def Parser.upper : Parser Char := sat Char.isUpper

#guard upper "ABC" = some ('A', "BC")
#guard upper "abc" = none

/-- アルファベット用のパーサ -/
def Parser.letter : Parser Char := sat Char.isAlpha

#guard letter "abc" = some ('a', "bc")
#guard letter "123" = none
#guard letter "ABC" = some ('A', "BC")

/-- アルファベットまたは数字用のパーサ -/
def Parser.alphanum : Parser Char := sat Char.isAlphanum

#guard alphanum "abc" = some ('a', "bc")
#guard alphanum "123" = some ('1', "23")
#guard alphanum "!" = none

/-- 指定された文字用のパーサ -/
def Parser.char (c : Char) : Parser Char := sat (· = c)

#guard char 'a' "abc" = some ('a', "bc")
#guard char 'a' "123" = none

/-- 指定された文字列用のパーサ -/
def Parser.string (x : String) : Parser String :=
  match x with
  | ⟨[]⟩ => pure ""
  | ⟨x :: xs⟩ => do
    let z ← char x
    let zs ← string ⟨xs⟩
    return s!"{z}" ++ zs

#guard string "abc" "abcdef" = some ("abc", "def")
#guard string "abc" "ab123" = none

mutual
  def Alternative.many {α : Type} {m : Type → Type} [Alternative m] (x : m α) : m (List α) :=
    some x <|> pure []

  def Alternative.some {α : Type} {m : Type → Type} [Alternative m] (p : m α) : m (List α) :=
    pure (· :: ·) <*> p <*> many p
end

def Parser.space : Parser Unit := do
  let _ sat Char.isWhitespace
  pure ()
