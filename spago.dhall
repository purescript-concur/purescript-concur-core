{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "concur-core"
, dependencies =
    [ "arrays"
    , "console"
    , "foldable-traversable"
    , "nonempty"
    , "profunctor-lenses"
    , "tailrec"
    , "event"
    ]
, license = "MIT"
, repository = "https://github.com/purescript-concur/purescript-concur-core"
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
