{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "argonaut-generic"
  , "console"
  , "dotenv"
  , "effect"
  , "node-fs-aff"
  , "node-process"
  , "parsing"
  , "psci-support"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "simple-ajax"
  , "simple-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
