module Environment where

import Effect (Effect)

foreign import getApiKey :: Effect String

foreign import getRandomInt :: Int -> Int -> Effect Int