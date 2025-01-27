import Lake

open Lake DSL

package «ProgrammingHaskell» where
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩,
    ⟨`linter.missingDocs, true⟩
  ]

require batteries from git
  "https://github.com/leanprover-community/batteries" @ "main"
  
lean_exe countdown where
  root := `ProgrammingHaskell.Chapter09.Section7

lean_exe countdown_2 where
  root := `ProgrammingHaskell.Chapter09.Section8Exe


@[default_target]
lean_lib ProgrammingHaskell where
  globs := #[.submodules `ProgrammingHaskell, .submodules `MyStd]
