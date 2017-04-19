module AjaxExample.Todos where

import Prelude (discard)
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Aff (attempt)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Either (Either(Left, Right), either)
import Data.Foldable (for_)
import Data.Function (($), (<<<), const)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Network.HTTP.Affjax (AJAX, get)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (key)
import Text.Smolder.HTML (button, div, h1, li, ol)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!), (#!), text)

-- | Because AJAX is effectful and asynchronous, we represent requests and
-- | responses as input events.
data Event = RequestTodos | ReceiveTodos (Either String Todos)

type State =
  { todos :: Todos
  , status :: String }

type Todos = Array Todo

newtype Todo = Todo
  { id :: Int
  , title :: String }

init :: State
init = { todos: [], status: "Nothing loaded from server yet" }

-- | Decode our Todo JSON we receive from the server
instance decodeJsonTodo :: DecodeJson Todo where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    title <- obj .? "title"
    pure $ Todo { id: id, title: title }

-- | Our update function requests data from the server, and handles data
-- | received from input.
foldp :: Event -> State -> EffModel State Event (ajax :: AJAX)
foldp (ReceiveTodos (Left err)) state =
  noEffects $ state { status = "Error fetching todos: " <> show err }
foldp (ReceiveTodos (Right todos)) state =
  noEffects $ state { todos = todos, status = "Todos" }
foldp (RequestTodos) state =
  { state: state { status = "Fetching todos..." }
  , effects: [ do
      res <- attempt $ get "http://jsonplaceholder.typicode.com/users/1/todos"
      let decode r = decodeJson r.response :: Either String Todos
      let todos = either (Left <<< show) decode res
      pure $ Just $ ReceiveTodos todos
    ]
  }

view :: State -> HTML Event
view state =
  div do
    h1 $ text state.status
    div do
      button #! onClick (const RequestTodos) $ text "Fetch todos"
      ol $ for_ state.todos todo

todo :: Todo -> HTML Event
todo (Todo state) =
  li ! key (show state.id) ! className "todo" $ text state.title
