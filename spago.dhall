{ name = "polaris-codegen"
, dependencies =
  [ "affjax"
  , "arrays"
  , "argonaut-codecs"
  , "console"
  , "cst-simple"
  , "effect"
  , "foreign-object"
  , "node-fs-aff"
  , "parsing"
  , "ps-cst"
  , "simple-json"
  , "strings-extra"
  , "psci-support"
  , "st"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
