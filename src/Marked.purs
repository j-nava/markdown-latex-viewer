module Marked where

import Effect (Effect)

foreign import parse :: String -> Effect String
