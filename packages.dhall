let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210409/packages.dhall sha256:e81c2f2ce790c0e0d79869d22f7a37d16caeb5bd81cfda71d46c58f6199fd33f

let additions =
      { ps-cst =
        { dependencies =
          [ "console"
          , "effect"
          , "psci-support"
          , "record"
          , "strings"
          , "spec"
          , "node-path"
          , "node-fs-aff"
          , "ansi"
          , "dodo-printer"
          ]
        , repo = "https://github.com/purescript-codegen/purescript-ps-cst.git"
        , version = "1339dd3"
        }
      , cst-simple =
        { dependencies =
          [ "arrays"
          , "console"
          , "debug"
          , "effect"
          , "node-fs-aff"
          , "ps-cst"
          , "psci-support"
          , "spec"
          , "string-parsers"
          , "typelevel-prelude"
          , "unicode"
          ]
        , repo =
            "https://github.com/purescript-codegen/purescript-cst-simple.git"
        , version = "1fa8cd2"
        }
      }

in  upstream // additions
