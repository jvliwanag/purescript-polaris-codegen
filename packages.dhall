let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210406/packages.dhall sha256:7b6af643c2f61d936878f58b613fade6f3cb39f2b4a310f6095784c7b5285879

let additions =
      { dodo-printer =
        { dependencies =
          [ "aff"
          , "ansi"
          , "avar"
          , "console"
          , "effect"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "minibench"
          , "node-child-process"
          , "node-fs-aff"
          , "node-process"
          , "psci-support"
          , "strings"
          ]
        , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
        , version = "v2.0.0"
        }
      , ps-cst =
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
      , string-extra =
        { dependencies =
          [ "console"
          , "assert"
          , "arrays"
          , "unicode"
          , "foldable-traversable"
          , "either"
          , "partial"
          , "maybe"
          , "strings"
          , "prelude"
          ]
        , repo =
            "https://github.com/purescript-contrib/purescript-strings-extra.git"
        , version = "v3.0.0"
        }
      }

in  upstream // additions
