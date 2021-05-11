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
  , "debug"
  , "foldable-traversable"
  , "free"
  , "js-timers"
  , "nonempty"
  , "profunctor-lenses"
  , "tailrec"
  ]
, license = "MIT"
, repository = "https://github.com/purescript-concur/purescript-concur-core"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
