# Change Log

## [v7.0] - 2016-10-24

### Changed

- Switched version scheme from [SemVer](http://semver.org/) to [ComVer](https://github.com/staltz/comver)
- Updated for latest PureScript version 0.10.1

### Added

- Additional HTML element constructors.
  [#74](https://github.com/alexmingoia/purescript-pux/pull/74)

### Removed

- Remove bind/append functions in favor of operators. Fixes bug with `Html` and
  `Functor` instance.
  [#72](https://github.com/alexmingoia/purescript-pux/pull/72)

## [v6.0.1] - 2016-09-25

### Fixed

- Fix typo with `withTextChild` and `withChildren` precendence.
  [#70](https://github.com/alexmingoia/purescript-pux/pull/70)

## [v6.0.0] - 2016-09-21

### Changed

- Tweak fixity of `Html` operators
  [#64](https://github.com/alexmingoia/purescript-pux/pull/64)

### Added

- Export `actionChannel` with `App`
  [#66](https://github.com/alexmingoia/purescript-pux/pull/66)
- Support `event.target.checked`
  [#62](https://github.com/alexmingoia/purescript-pux/pull/62)

## [v5.0.3] - 2016-07-20

#### Changed

- Allow EXCEPTION effect in updates
  [#58](https://github.com/alexmingoia/purescript-pux/pull/58)

#### Fixed

- Invoke React.createElement correctly when passing it children
  [#51](https://github.com/alexmingoia/purescript-pux/pull/51)
- Various docs updated for purescript 0.9

## [v5.0.2] - 2016-07-09

#### Fixed

- Correctly parse queries in `routeFromUrl`
  [#49](https://github.com/alexmingoia/purescript-pux/pull/49)

#### Added

- Implement attribute functor
  [#47](https://github.com/alexmingoia/purescript-pux/pull/46)

## [v5.0.1] - 2016-06-25

#### Fixed

- Add missing purescript-globals dependency
  [#46](https://github.com/alexmingoia/purescript-pux/issues/46)
- Use <> instead of ++ and pure instead of return

## [v5.0.0] - 2016-06-22

- Update dependencies for PureScript 0.9 - no changes to the public API.

## [v4.1.0] - 2016-05-15

### Added

- `onlyEffects` function for returning an `effModel` with new effects but no
  state changes.

### Fixed

- Fix map FFI for `Html` text nodes.
  [#38](https://github.com/alexmingoia/purescript-pux/issues/38)

## [v4.0.1] - 2016-04-27

### Fixed

- Fix map of actions attached to app's root element. See
  [#4e68bc8](https://github.com/alexmingoia/purescript-pux/commit/4e68bc845fe9a58df59389a3c42c9d7db5ce88ca)

## [v4.0.0] - 2016-04-21

### Breaking changes

- `style` attribute changed to accept an array of tuples instead of a record.
  This is so styles can be composed. See
  [#27](https://github.com/alexmingoia/purescript-pux/issues/27)
- Change `toReact` to return `ReactClass props` from purescript-react.

## [v3.1.0] - 2016-04-18

### Fixed

- Change `tabIndex` to `Int -> Attribute a`.
- Change `checked` to `Boolean -> Attribute a`.
- Do not call `.preventDefault()` on click, change, and other relevant handlers.

### Added

- `Pux.Html.Attributes.defaultValue` and `Pux.Html.Attributes.defaultChecked`.
- `Pux.Html.Events.onKey` for sending an action when a specific key is pressed.
- `Pux.Router.navigateTo` for changing the URL.

### Changed

- Export app `Config` type.

## [v3.0.0] - 2016-04-09

### Fixed

- BREAKING: Change relevant attributes to `Boolean -> Attribute a` or
  `Int -> Attribute a`.

### Changed

- Implement functor instance for `Html`, so authors can use `map` instead of
  `forwardTo`.

### Added

- Provide type alias for core set of effects `CoreEffects`.
  See [#20](https://github.com/alexmingoia/purescript-pux/pull/20).
- Added
  [`toReact`](https://alexmingoia.github.com/purescript-pux/docs/react-interop.html)
  function that returns React class from Pux component.
- Added
  [`fromReact`](https://alexmingoia.github.com/purescript-pux/docs/react-interop.html)
  function that returns Pux Html element from React component.
- Added additional SVG attributes and elements from React v15.0.0

## [v2.0.2] - 2016-04-02

### Fixed

- Use record instead of string for style attribute value.

## [v2.0.1] - 2016-03-29

- Fix race-condition for asynchronous effects
  [#15](https://github.com/alexmingoia/purescript-pux/issues/15).
- Fix recursive/nested `forwardTo`
  [#14](https://github.com/alexmingoia/purescript-pux/issues/14).

## [v2.0.0] - 2016-03-27

- All functions in `Pux.Html.Elements` now receive children. This enables
  consistent use of the `!` operator.
- Added `##` operator to combine array of `Html`.

## [v1.0.0] - 2016-03-24

### Breaking Changes

- The `VirtualDOM` monad has been replaced with `#` operator and rebindable
  `do`, which enables the use of both array notation or `do` notation for
  composing views.
- `VirtualDOM` is now `Html a` and is parameterized by the component's action
  type.
- One-to-one mapping between event types and React's synthetic events.
- The `Update` function no longer receives an input `Signal.Channel`.
- `EffModel` type has changed to
  `{ state: state, effects: Array (Aff eff action)`, and actions are
   automatically fed into the input channel.

### Upgrading

- The `VirtualDOM` monad has been replaced by a simpler data structure `Html a`.
  Element attributes and children can now be specified using array literal
  notation. `do` notation is optional and can be used via `Pux.Html.bind`
  and `Pux.Html.#`.
- All event handlers receive an event, and the use of `Prelude.const` is
  encouraged to ignore events when constructing actions.
- The effect in the `Update` function type signature now appears last, and
  `Update` no longer receives a channel as the third argument.
- Applications are now created using `start`, and the resulting `app.html`
  signal passed to `renderToDOM` or `renderToString`.

## [v0.3.0] - 2016-03-24

### Fixed

- Load React from CommonJS. Fixes [#10](https://github.com/alexmingoia/purescript-pux/issues/10).
- Fix onChange handler so it produces a value. Fixes [#7](https://github.com/alexmingoia/purescript-pux/issues/7).
- List all dependencies. Fixes [#9](https://github.com/alexmingoia/purescript-pux/issues/9).

### Added

- SVG elements and attributes. Fixes [#6](https://github.com/alexmingoia/purescript-pux/issues/6).

## [v0.1.0] - 2016-02-02

### Fixed
- Make foreign functions safe in runtimes without `window`.

### Changed
- BREAKING: `View` type synonym removed. `VDom` renamed to `VirtualDOM` and
exported via `Pux` and `Pux.DOM` instead of `Pux.View`.

## [v0.0.1] - 2016-01-24

### Added
- Initial release. Experimental API.
