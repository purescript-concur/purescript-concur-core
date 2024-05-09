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
  , "identity"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor-lenses"
  , "refs"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unsafe-reference"
  ]
, license = "MIT"
, repository = "https://github.com/purescript-concur/purescript-concur-core"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
