{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "concur-core"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "console"
  , "foldable-traversable"
  , "free"
  , "js-timers"
  , "profunctor-lenses"
  , "tailrec"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "identity"
  , "lazy"
  , "maybe"
  , "newtype"
  , "prelude"
  , "refs"
  , "transformers"
  , "tuples"
  ]
, license = "MIT"
, repository = "https://github.com/purescript-concur/purescript-concur-core"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
