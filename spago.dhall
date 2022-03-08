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
