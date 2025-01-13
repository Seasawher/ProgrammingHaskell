import Lake
open Lake DSL

package «ProgrammingHaskell» where
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩,
    ⟨`linter.missingDocs, true⟩
  ]

lean_exe countdown where
  root := `ProgrammingHaskell.Chapter09.Section7

lean_exe countdown_2 where
  root := `ProgrammingHaskell.Chapter09.Section8Exe

@[default_target]
lean_lib ProgrammingHaskell where
  globs := #[.submodules `ProgrammingHaskell]
