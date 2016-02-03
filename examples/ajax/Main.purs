module Main where

import Prelude hiding (div)

import Control.Bind
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.List (singleton)
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Network.HTTP.Affjax
import Pux
import Pux.DOM.HTML.Elements (div, p, button, text)
import Pux.DOM.HTML.Attributes (onClick, send)
import Pux.Render.DOM
import qualified Signal.Channel as S

-- | Because AJAX is effectful and asynchronous, we represent requests and
-- | responses as input actions.
data Action = RequestData | ReceiveData State

-- | Our state describes the server response, along with instances for
-- | deriving state from JSON.
data State = State { message :: String }

instance showState :: Show State where
  show (State o) = "{ \"message\": \"" ++ o.message ++ "\" }"

instance stateIsForeign :: IsForeign State where
  read value = do
    message <- readProp "message" value
    return $ State { message: message }

initialState :: State
initialState = State { message: "Nothing loaded from server yet." }

-- | Our update function requests data from the server, and handles data
-- | received from input.
update :: forall eff. Update
          (ajax :: AJAX, err :: EXCEPTION, console :: CONSOLE| eff)
          State
          Action
update action state input =
  case action of
    (ReceiveData newState) ->
      { state: newState
      , effects: [ do log $ "Updated new state: " ++ (show newState) ]
      }
    RequestData ->
      { state: State { message: "Loading data from server..." }
      , effects:
        [ launchAff $ do
            res <- get "./data.json"
            let newState = readJSON res.response :: F State
            liftEff $ case newState of
              (Left err) -> log "Error parsing JSON!"
              (Right newState) -> S.send input (singleton (ReceiveData newState))
        ]
      }

view :: State -> VirtualDOM
view (State state) = div $ do
  p $ text state.message
  p $ do
    button ! onClick (send RequestData) $ text "Fetch data"

main = renderToDOM "#app" =<< app
  { state: initialState
  , update: update
  , view: view
  , inputs: []
  }
