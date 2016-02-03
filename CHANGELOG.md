# Change Log

## [v0.1.0] - 2016-02-02

No changes to the API are expected after this release. Only the addition of SVG
elements is planned for v1.0.0

### Fixed
- Make foreign functions safe in runtimes without `window`.

### Changed
- BREAKING: `View` type synonym removed. `VDom` renamed to `VirtualDOM` and
exported via `Pux` and `Pux.DOM` instead of `Pux.View`.

## [v0.0.1] - 2016-01-24

### Added
- Initial release. Experimental API.
