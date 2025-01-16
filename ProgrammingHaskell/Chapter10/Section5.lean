/-- 簡略化された `getLine` 関数。
`Stream` を引数に取る必要がなく、返り値はトリム済みになる。-/
def IO.getln : IO String := do
  let input ← (← IO.getStdin).getLine
  return input.trim

/-- キーボードから文字列を読み込んで、その長さを表示する -/
def main : IO Unit := do
  IO.print "Enter a string: "
  let input ← IO.getln
  IO.println s!"The string has {input.trim.length} characters."
