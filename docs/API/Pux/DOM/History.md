## Module Pux.DOM.History

#### `sampleURL`

``` purescript
sampleURL :: forall eff. Window -> Eff ("channel" :: CHANNEL, "history" :: HISTORY, "dom" :: DOM | eff) (Signal String)
```

Returns a signal containing the current window.location pathname and search query,
which is updated on every "popstate" event.


