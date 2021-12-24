module SanitizeHtml where

import Effect (Effect)

foreign import sanitize :: String -> Effect String
