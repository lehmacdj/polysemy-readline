# polysemy-readline
[![GitHub Actions](https://github.com/lehmacdj/polysemy-readline/actions/workflows/ci.yml/badge.svg)](https://github.com/lehmacdj/polysemy-readline/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/polysemy-readline.svg?logo=haskell)](https://hackage.haskell.org/package/polysemy-readline)

This package provides a [polysemy](https://github.com/polysemy-research/polysemy#readme) effect that provides most of the functionality of [haskeline](https://github.com/judah/haskeline#readme). See Haskeline's documentation for additional usage information.

## Contributions
Bug reports and PRs are welcome. In particular there are a number of things that I don't use frequently enough from Haskeline to justify working on or just haven't gotten around to implementing yet:
- interrupt handling: `withInterrupt`, `handleInterrupt`
- pure interpreter for use in tests
- support for older versions of Haskeline (currently only 0.8.1+ is supported)
- version bumps for dependencies that already compile but aren't allowed

PRs for any of these things would be greatly appreciated or I will try to get around to implementing them myself later 🙂.
