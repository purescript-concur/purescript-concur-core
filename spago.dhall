{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "concur-core"
, dependencies =
  [ "aff"
  , "aff-bus"
  , "arrays"
  , "avar"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "free"
  , "identity"
  , "lazy"
  , "maybe"
  , "newtype"
  , "parallel"
  , "prelude"
  , "profunctor-lenses"
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, license = "MIT"
, repository = "https://github.com/purescript-concur/purescript-concur-core"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
