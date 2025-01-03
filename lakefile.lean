import Lake
open Lake DSL

package «ProgrammingHaskell» where
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩,
    ⟨`linter.missingDocs, true⟩
  ]

lean_exe countdown where
  root := `ProgrammingHaskell.Part1.Chapter09.Section7

@[default_target]
lean_lib ProgrammingHaskell where
  globs := #[.submodules `ProgrammingHaskell]
