# Forms

> [Example code](https://github.com/alexmingoia/purescript-pux/tree/master/examples/forms/)

Handling forms is central to many web applications. Forms can be encapsulated
into a component which handles form state, validation, and persistence. We will
build a simple login component to demonstrate.

Our state is simple and need only keep track of the form input:

```purescript
type State =
  { username :: String
  , password :: String }

data Action
  = SignIn
  | UsernameChange FormEvent
  | PasswordChange FormEvent
```

Our login component has three actions. `UsernameChange` and `PasswordChange`
are events which happen every time the inputs change, and receive a
[`FormEvent`](/API/Pux/Html/Events.html#formevent) containing event
information. The `SignIn` action is sent when the form is submitted, and is
meant to be handled by the parent component to deal with authentication.

```purescript
update :: Action -> State -> State
update (SignIn) state = state
update (UsernameChange ev) state = state { username = ev.target.value }
update (PasswordChange ev) state = state { password = ev.target.value }
```

Our update function responds to changes from the form inputs. This is where any
validation logic would go.

Lastly, we expose a view which displays the username and password inputs, along
with a button to submit the form:

```purescript
view state = form
  [ name "signin"
  , onSubmit (const SignIn)
  ]
  [ input [ type_ "text", value state.username, onChange UsernameChange ] []
  , input [ type_ "password", value state.password, onChange PasswordChange ] []
  , button [ type_ "submit" ] [ text "Sign In" ]
  ]
```
