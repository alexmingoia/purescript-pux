module RoutingExample.Routes where

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Data.Functor ((<$), (<$>))
import Data.Function (($))
import Data.Maybe (fromMaybe)
import Pux.Router (param, router, lit, int, end)

data Route = Home | Users String | User Int | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
  Home <$ end
  <|>
  Users <$> (lit "users" *> param "sortBy") <* end
  <|>
  Users "name" <$ (lit "users") <* end
  <|>
  User <$> (lit "users" *> int) <* end
