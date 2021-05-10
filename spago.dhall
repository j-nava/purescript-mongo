{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "mongo"
, dependencies =
  [ "effect", "aff", "simple-json", "psci-support", "node-process", "bifunctors", "either", "exceptions", "foreign", "functions", "maybe", "nullable", "prelude", "record", "typelevel-prelude", "unsafe-coerce" ]
, packages = ./packages.dhall
}
