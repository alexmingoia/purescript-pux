module FormsExample.Form where

import Prelude (const)
import Pux.Html (Html, form, span, button, text, input)
import Pux.Html.Events (onSubmit, onChange, FormEvent)
import Pux.Html.Attributes (name, type_, value)

type State =
  { username :: String
  , password :: String }

data Action
  = SignIn
  | UsernameChange FormEvent
  | PasswordChange FormEvent

init :: State
init = { username: "user", password: "pass" }

update :: Action -> State -> State
update (SignIn) state = state
update (UsernameChange ev) state = state { username = ev.target.value }
update (PasswordChange ev) state = state { password = ev.target.value }

view :: State -> Html Action
view state = form
  [ name "signin"
  , onSubmit (const SignIn)
  ]
  [ input [ type_ "text", value state.username, onChange UsernameChange ] []
  , input [ type_ "password", value state.password, onChange PasswordChange ] []
  , button [ type_ "submit" ] [ text "Sign In" ]
  ]

