module FormsExample.Form where

import Prelude (discard)
import Data.Function (($), const)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent, onSubmit, onChange, targetValue)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (button, form, input)
import Text.Smolder.HTML.Attributes (name, type', value)
import Text.Smolder.Markup ((!), (#!), text)

type State =
  { username :: String
  , password :: String }

data Event
  = SignIn
  | UsernameChange DOMEvent
  | PasswordChange DOMEvent

init :: State
init = { username: "user", password: "pass" }

foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp (SignIn) st = noEffects st
foldp (UsernameChange ev) st = noEffects $ st { username = targetValue ev }
foldp (PasswordChange ev) st = noEffects $ st { password = targetValue ev }

view :: State -> HTML Event
view state =
  form ! name "signin" #! onSubmit (const SignIn) $ do
    input ! type' "text" ! value state.username #! onChange UsernameChange
    input ! type' "password" ! value state.password #! onChange PasswordChange
    button ! type' "submit" $ text "Sign In"

