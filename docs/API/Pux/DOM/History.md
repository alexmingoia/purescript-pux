## Module Pux.DOM.History

#### `sampleURL`

``` purescript
sampleURL :: Window -> Effect (Signal String)
```

Returns a signal containing the current window.location pathname and search query,
which is updated on every "popstate" event.


