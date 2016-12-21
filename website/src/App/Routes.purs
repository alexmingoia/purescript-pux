module App.Routes where

import Control.Alternative ((<|>))
import Data.Function (($))
import Data.Functor ((<$))
import Data.Generic (class Generic, gShow)
import Data.Maybe (fromMaybe)
import Data.Show (class Show)
import Pux.Router (end, lit, router)

data Route
  = Home String
  | Guide String
  | NotFound String

derive instance genericRoute :: Generic Route

instance showRoute :: Show Route where
  show = gShow

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Home url <$ end
  <|>
  Guide url <$ lit "docs"
