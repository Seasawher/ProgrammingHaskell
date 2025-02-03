import Plausible

instance : Monad List where
  pure := List.singleton
  bind := List.flatMap

namespace Map
  /- ## map の再帰を使う定義と、使わない定義を比較する -/

  variable {α β : Type}

  /-- do 構文による map の実装 -/
  def List.doMap (f : α → β) (xs : List α) : List β := do
    let x ← xs
    return f x

  -- テスト
  #test
    ∀ {α β : Type} (f : α → β) (xs : List α),
    List.doMap f xs = xs.map f

end Map

universe u

instance instAlternative : Alternative List.{u} where
  failure := @List.nil
  orElse l l' := List.append l (l' ())

namespace Filter

  /- ## filter の再帰を使う定義と、使わない定義を比較する -/

  variable {α : Type}

  /-- do 構文による filter の実装 -/
  def List.doFilter (p : α → Bool) (xs : List α) : List α := do
    let x ← xs
    guard <| p x
    return x

  -- テスト
  #test
    ∀ {α : Type} (p : α → Bool) (xs : List α),
    List.doFilter p xs = List.filter p xs

end Filter

def List.sseven (xs : List Nat) : Nat :=
  xs.filter (· % 2 = 0)
    |>.map (· ^ 2)
    |>.sum

#guard [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].sseven = [2, 4, 6, 8, 10].sseven
