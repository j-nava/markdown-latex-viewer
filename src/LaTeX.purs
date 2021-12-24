module LaTeX where

import Prelude

import Control.Alt ((<|>))
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Tuple (fst)
import Effect (Effect)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Replace.String (anyTill, splitCap)

replace :: String -> Effect String
replace text = do

  foldM (\a i -> append a <$> resolveSection i) "" splitResult

  where
  
    pattern = 
      (string "$$" *> anyTill (string "$$"))
      <|> (string "$" *> anyTill (string "$"))

    splitFn = 
      bimap identity fst <$> splitCap pattern text

    splitResult = bimap identity identity <$> splitFn

    resolveSection = case _ of
      Left l -> pure l
      Right r -> 
        parse r >>= case _ of
          Right s -> pure s
          Left _ -> pure r


parse :: String -> Effect (Either String String)
parse text = _parse text Left Right 

foreign import _parse :: String -> (String -> Either String String) -> (String -> Either String String) -> Effect (Either String String)
