import Lake
open Lake DSL

package «ProgrammingHaskell»

@[default_target]
lean_lib ProgrammingHaskell where
  globs := #[.submodules `ProgrammingHaskell]
