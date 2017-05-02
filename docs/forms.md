# Forms

Handling forms is central to many web applications. We will build a simple
login form to demonstrate.

First, state and event types are needed to keep track of changes to form input:

```purescript
type State =
  { username :: String
  , password :: String }

data Event
  = SignIn
  | UsernameChange DOMEvent
  | PasswordChange DOMEvent
```

`UsernameChange` and `PasswordChange` events happen every time the inputs
change, and receive a
[DOM `Event`](https://pursuit.purescript.org/packages/purescript-dom/4.3.1/docs/DOM.Event.Types#t:Event)
from
[purescript-dom](https://pursuit.purescript.org/packages/purescript-dom/4.3.1).
The `SignIn` event is triggered when the form is submitted.

```purescript
foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp (SignIn) state =
  { state, effects: [ log "sign in" *> pure Nothing ] }
foldp (UsernameChange ev) state =
  { state: state { username = targetValue ev, effects: [] }
foldp (PasswordChange ev) state =
  { state: state { password = targetValue ev, effects: [] }
```

The raw DOM event is used to get the value of the inputs. Pux provides
[`targetValue`](https://pursuit.purescript.org/packages/purescript-pux/9.0.0/docs/Pux.DOM.Events#v:targetValue)
which takes a `DOMEvent` and returns the value of `ev.target.value`.

The view uses `onChange` and `onSubmit` handlers from
[`Pux.DOM.Events`](https://pursuit.purescript.org/packages/purescript-pux/9.0.0/docs/Pux.DOM.Events)
to hook application events to DOM events:

```purescript
view :: State -> HTML Event
view state =
  form ! name "signin" #! onSubmit (const SignIn) $ do
    input ! type' "text" ! value state.username #! onChange UsernameChange
    input ! type' "password" ! value state.password #! onChange PasswordChange
    button ! type' "submit" $ text "Sign In"
```

> #### Next: [Routing](/docs/routing)
> #### Previous: [Components](/docs/components)
