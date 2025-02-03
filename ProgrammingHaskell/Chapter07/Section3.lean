import Plausible

namespace Foldr
  /- ## foldr で書ける関数たち -/
  variable {α β : Type}

  /-- 和の計算 -/
  def sum [Zero α] [Add α] (xs : List α) : α :=
    match xs with
    | [] => 0
    | x :: xs => x + sum xs
  #check sum

  -- インスタンス暗黙引数を使うことはできない？？
  #guard_msgs (drop error) in
    #test ∀{α : Type}[Zero α][Add α](xs : List α), sum xs = xs.foldr (· + ·) 0

  example [Zero α] [Add α] (xs : List α) : sum xs = xs.foldr (· + ·) 0 := by
    -- plausible はなぜ使えないのだろうか？
    fail_if_success plausible

    sorry

  -- `α := Nat` とすれば使える
  #test ∀ (xs : List Nat), sum xs = xs.foldr (· + ·) 0

end Foldr
