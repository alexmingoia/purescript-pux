module Main where

import Prelude hiding (div)

import Control.Alt ((<|>))
import Control.Apply
import Control.Bind
import Data.Maybe
import Data.Functor
import Pux
import Pux.Render.DOM
import Pux.DOM.HTML.Elements (div, ul, li, a, h1, text)
import Pux.Router
import Signal ((~>))

data Action = PageView Route
data Route = Home | Users | User Int | NotFound

type State = { currentRoute :: Route }

match :: String -> Action
match url = PageView $ fromMaybe NotFound $ router url $
  Home <$ end
  <|>
  Users <$ (lit "users") <* end
  <|>
  User <$> (lit "users" *> int) <* end

update :: forall eff. Update eff State Action
update action state input = case action of
  (PageView route) ->
    { state: { currentRoute: route }
    , effects: []
    }

view :: State -> VirtualDOM
view state = div $ do
  div $ case state.currentRoute of
    Home -> h1 $ text "Home"
    Users -> h1 $ text "Users"
    (User id)-> h1 $ text ("User: " ++ show id)
    _ -> h1 $ text "Not Found"
  ul $ do
    li $ a ! link "/" $ text "Home"
    li $ a ! link "/users" $ text "Users"
    li $ a ! link "/users/123" $ text "User 123"
    li $ a ! link "/foobar" $ text "Not found"

main = do
  urlSignal <- sampleUrl
  let routeSignal = urlSignal ~> match

  renderToDOM "#app" =<< app
    { state: { currentRoute: Home }
    , update: update
    , view: view
    , inputs: [routeSignal]
    }
