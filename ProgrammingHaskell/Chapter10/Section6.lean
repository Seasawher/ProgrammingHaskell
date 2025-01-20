import MyStd.IO

def sgetLine : IO String := do
  let x ← IO.getInput
  if x == "\n" then
    return ""
  IO.print "-"
  return x

def hangman : IO Unit := do
  IO.println "Think of a word:"

