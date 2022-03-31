{ name = "wags-graph-builder"
, dependencies =
  [ "console"
  , "control"
  , "effect"
  , "indexed-monad"
  , "prelude"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
