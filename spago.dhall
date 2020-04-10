{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "digraph"
  , "effect"
  , "foldable-traversable"
  , "format"
  , "parsing"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "spec-quickcheck"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
