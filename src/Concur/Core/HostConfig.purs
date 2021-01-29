module Concur.Core.HostConfig where


data Props

data ClassInst a
data FragInst a
data TextInst a

{-
class ConcurHost m i f t where
  createInstance :: String -> Props -> i
  createDocumentFragment :: f
  createTextNode :: String -> t
  createNumberNode :: Number -> t
  insertBefore :: i -> i -> i -> m Unit
  appendChild :: i -> Array i -> m Unit
  removeSelf :: i -> m Unit
  removeAllChild :: i -> m Unit
  updateProps :: i -> Props -> Props -> m Unit
-}
