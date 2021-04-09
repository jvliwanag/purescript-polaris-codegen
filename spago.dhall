{ name = "polaris-codegen"
, dependencies =
  [ "arrays"
  , "console"
  , "cst-simple"
  , "effect"
  , "foreign-object"
  , "node-fs-aff"
  , "parsing"
  , "ps-cst"
  , "simple-json"
  , "string-extra"
  , "psci-support"
  , "st"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
