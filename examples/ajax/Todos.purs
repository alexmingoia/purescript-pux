module AjaxExample.Todos where

import Control.Monad.Aff (attempt)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Either (Either(Left, Right), either)
import Network.HTTP.Affjax (AJAX, get)
import Prelude (($), bind, map, const, show, (++), return, (<<<))
import Pux (EffModel, noEffects)
import Pux.Html (Html, div, h1, ol, li, button, text)
import Pux.Html.Attributes (key, className)
import Pux.Html.Events (onClick)

-- | Because AJAX is effectful and asynchronous, we represent requests and
-- | responses as input actions.
data Action = RequestTodos | ReceiveTodos (Either String Todos)

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
    return $ Todo { id: id, title: title }

-- | Our update function requests data from the server, and handles data
-- | received from input.
update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (ReceiveTodos (Left err)) state =
  noEffects $ state { status = "Error fetching todos: " ++ show err }
update (ReceiveTodos (Right todos)) state =
  noEffects $ state { todos = todos, status = "Todos" }
update (RequestTodos) state =
  { state: state { status = "Fetching todos..." }
  , effects: [ do
      res <- attempt $ get "http://jsonplaceholder.typicode.com/users/1/todos"
      let decode r = decodeJson r.response :: Either String Todos
      let todos = either (Left <<< show) decode res
      return $ ReceiveTodos todos
    ]
  }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text state.status ]
    , div
        []
        [ button [ onClick (const RequestTodos) ] [ text "Fetch todos" ]
        , ol [] $ map todo state.todos
        ]
    ]

todo :: Todo -> Html Action
todo (Todo state) =
  li [ key (show state.id), className "todo" ] [ text state.title ]
