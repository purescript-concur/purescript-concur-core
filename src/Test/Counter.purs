module Test.Counter where

import Prelude

import Concur.React (HTML, Widget)
import Concur.React.DOM (text, div', p')
import Concur.React.Widgets (textButton')
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)

counterWidget :: forall a eff. Int -> Widget HTML (console :: CONSOLE | eff) a
counterWidget count = do
  n <- div'
        [ p' [text ("State: " <> show count)]
        , textButton' "Increment" $> count+1
        , textButton' "Decrement" $> count-1
        ]
  liftEff (log ("COUNT IS NOW: " <> show n))
  counterWidget n