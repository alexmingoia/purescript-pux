module RoutingExample.Routes where

import Prelude (($), (<$>))
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Data.Maybe (fromMaybe)
import Data.Functor ((<$))
import Pux.Router (router, lit, int, end)

data Route = Home | Users | User Int | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
  Home <$ end
  <|>
  Users <$ (lit "users") <* end
  <|>
  User <$> (lit "users" *> int) <* end
