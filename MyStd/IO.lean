/- # Lean の標準ライブラリにあってほしい関数の定義 -/

/-- Accepts a string input from the keyboard, including the trailing newline as-is. -/
def IO.getInputln : IO String := do
  (← IO.getStdin).getLine

/-- Accepts a string input from the keyboard, removing newline characters. -/
def IO.getInput : IO String := do
  return removeBr (← getInputln)
where
  removeBr : String → String :=
    if System.Platform.isWindows then
      (String.replace · "\r" "") ∘ (String.replace · "\n" "")
    else
      (String.replace · "\n" "")
