{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut-core"
  , "arrays"
  , "codec-argonaut"
  , "console"
  , "datetime"
  , "dom-indexed"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-store"
  , "halogen-subscriptions"
  , "http-methods"
  , "integers"
  , "maybe"
  , "numbers"
  , "prelude"
  , "random"
  , "record"
  , "remotedata"
  , "routing-duplex"
  , "safe-coerce"
  , "strings"
  , "tailrec"
  , "tuples"
  , "typelevel-prelude"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
