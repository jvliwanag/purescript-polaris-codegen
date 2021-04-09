{ name = "polaris"
, dependencies =
  [ "console", "effect", "psci-support", "react-basic-hooks", "untagged-union" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
