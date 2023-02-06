{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "concur-core"
, dependencies =
  [ "arrays"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "free"
  , "identity"
  , "lazy"
  , "maybe"
  , "newtype"
  , "prelude"
  , "refs"
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, license = "MIT"
, repository = "https://github.com/purescript-concur/purescript-concur-core"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
