# polysemy-readline
[![GitHub Actions](https://github.com/lehmacdj/polysemy-readline/actions/workflows/ci.yml/badge.svg)](https://github.com/lehmacdj/polysemy-readline/actions/workflows/ci.yml)
[![Hackage](http://img.shields.io/hackage/v/polsyemy-readline.svg)](http://img.shields.io/hackage/v/polsyemy-readline.svg)

This package provides a [polysemy](https://github.com/polysemy-research/polysemy#readme) effect that provides most of the functionality of [haskeline](https://github.com/judah/haskeline#readme). See Haskeline's documentation for usage information.

## Contributions
Issues or PRs are welcome. In particular there are a number of things that I don't use frequently enough from Haskeline to justify working on or just haven't gotten around to implementing yet:
- interrupt handling: `withInterrupt`, `handleInterrupt`
- pure interpreter for using in tests
- additional interpreters matching the `run*` functions for `InputT`
- support for older versions of Haskeline (currently only 0.8.1+ is supported)
- version bumps for things that already compile but aren't allowed
PRs for any of these things would be greatly appreciated or I might get around to implementing them myself later 🙂.
