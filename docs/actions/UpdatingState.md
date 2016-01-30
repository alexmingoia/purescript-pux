# Updating State

## The `Update` function

All actions are combined into a single signal which is folded with the previous
state to produce a new state. This fold function is known as "update" or
"dispatch" and has type `Update eff State Action`.

`Update` receives an action, the current state, the input channel (for
asynchronous state changes), and returns a new state and collection of
effects to run.

{%purs%}
update :: forall eff. Update eff State Action
update action state input =
  case action of
    Increment ->
      { state: state { counter = state.counter + 1 }
      , effects: [] }
    Decrement ->
      { state: state { counter = state.counter - 1 }
      , effects: [] }
{%endpurs%}

## Effects

A series of effects can be run for any action. For example, logging to the
console:

{%purs%}
update :: forall eff. Update (console :: CONSOLE | eff) State Action
update action state input =
  case action of
    Increment ->
      { state: state { counter = state.counter + 1 }
      , effects: [ do log "increment" ] }
    Decrement ->
      { state: state { counter = state.counter - 1 }
      , effects: [ do log "decrement" ] }
{%endpurs%}

Another popular use case is AJAX requests:

{%purs%}
update :: forall eff. Update
    ( ajax :: AJAX
    , err :: EXCEPTION
    , console :: CONSOLE | eff) State Action
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
{%endpurs%}
