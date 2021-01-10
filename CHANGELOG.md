# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v6.1.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v6.1.0) - 2019-03-04

Added some functions to give better reporting abilities for pure QuickCheck runs

## [v6.0.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v6.0.0) - 2019-03-02

* Move guide over from documentation repo (@anttih)
* Bump dependencies

## [v5.0.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v5.0.0) - 2018-05-24

Updated for PureScript 0.12

## [v4.7.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v4.7.0) - 2018-03-30

- Added `quickCheckGen` and some variants that specialize the properties to be `Gen`-based (for convenience when using `MonadGen`-constrained properties)
- Added instances for `NonEmptyArray` and `NonEmptyString` (@matthewleon)

## [v4.6.2](https://github.com/purescript/purescript-quickcheck/releases/tag/v4.6.2) - 2018-02-26

- Fixed `resize` resetting seed (@matthewleon)

## [v4.6.1](https://github.com/purescript/purescript-quickcheck/releases/tag/v4.6.1) - 2017-12-20

- Fixed behaviour of `resize` (@safareli)

Previously `resize` would alter the state of `Gen` forever, rather than it being local to the `Gen` that was passed into `resize`. This may alter the behaviour of some existing tests if they relied on the buggy behaviour.

## [v4.6.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v4.6.0) - 2017-09-02

Add `Arbitrary` instance for records (@sharkdp)

## [v4.5.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v4.5.0) - 2017-09-01

Added some more helper comparison operations (@Risto-Stevcev)

## [v4.4.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v4.4.0) - 2017-06-18

- Added `Lazy` instance for `Gen`

## [v4.3.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v4.3.0) - 2017-06-03

Generic deriving for `Arbitrary` and `Coarbitrary` (@LiamGoodacre)

## [v4.2.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v4.2.0) - 2017-06-03

Add shuffle generator (@matthewleon)

## [v4.1.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v4.1.0) - 2017-05-28

- Added `MonadGen` instance

## [v4.0.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v4.0.0) - 2017-03-27

- Updated for PureScript 0.11
- Use `NonEmpty` for `Gen`s where called for (@matthewleon)
- Improved behaviour of `chooseInt` (@matthewleon)
- Added `Arbitrary` and `Coarbitrary` instances for `NonEmpty` and `NonEmptyList`  (@matthewleon)
- Added `enum` `Gen` for `BoundedEnum` values (@matthewleon)

## [v3.1.1](https://github.com/purescript/purescript-quickcheck/releases/tag/v3.1.1) - 2016-11-22

- Fixed shadowed variable warning

## [v3.1.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v3.1.0) - 2016-11-07

- Added `suchThat` combinator for `Gen`s

## [v3.0.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v3.0.0) - 2016-10-16

- Updated lists dependency

## [v2.0.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v2.0.0) - 2016-10-14

- Updated dependencies
- Stack safety improvement to `quickCheck` functions
- LCG clamping behaviour was fixed
- Seed is now printed for failing cases
- Tests can now be started with a specific seed

## [v1.0.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v1.0.0) - 2016-06-01

This release is intended for the PureScript 0.9.1 compiler and newer.

**Note**: The v1.0.0 tag is not meant to indicate the library is “finished”, the core libraries are all being bumped to this for the 0.9 compiler release so as to use semver more correctly.

## [v1.0.0-rc.2](https://github.com/purescript/purescript-quickcheck/releases/tag/v1.0.0-rc.2) - 2016-05-20

- Update dependencies and fix warnings

## [v1.0.0-rc.1](https://github.com/purescript/purescript-quickcheck/releases/tag/v1.0.0-rc.1) - 2016-03-27

- Release candidate for the psc 0.8+ core libraries

## [v0.12.2](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.12.2) - 2015-12-16

Fix `repeatable`.

## [v0.12.1](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.12.1) - 2015-11-19

- Fixed import warnings raised in psc 0.7.6

## [v0.12.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.12.0) - 2015-09-16

- Bumped dependencies
- Fixed unused type variable warnings

## [v0.11.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.11.0) - 2015-08-25

Bump `transformers` dependency to `0.7.1`. As such, this release requires version `0.7.4` of the PureScript compiler.

## [v0.10.1](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.10.1) - 2015-08-17

- Added `Arbitrary` and `Coarbitrary` instances for `Identity`, `Lazy`, and `List`.

## [v0.10.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.10.0) - 2015-08-16

- `Gen` should now be stack safe (@hdgarrood)

## [v0.9.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.9.0) - 2015-08-13

Updated dependencies

## [v0.8.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.8.0) - 2015-08-10

- Fixed poorly behaving LCG. This also involved some breaking changes to the interface (a newtype for `Seed` for one) (@hdgarood)
- Updated dependencies

## [v0.7.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.7.0) - 2015-07-17

Modify `QC` type synonym to work around type class instance bug in PSCi.

## [v0.6.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.6.0) - 2015-06-30

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

## [v0.6.0-rc.2](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.6.0-rc.2) - 2015-06-18

Fix LCG function.

## [v0.6.0-rc.1](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.6.0-rc.1) - 2015-06-10

Initial release candidate of the library intended for the 0.7 compiler.

## [v0.5.2](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.5.2) - 2015-04-04

- Added `Arbitrary` and `CoArbitrary` instances for `Unit` and `Ordering` (@garyb)

## [v0.5.1](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.5.1) - 2015-03-20

Updated docs

## [v0.5.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.5.0) - 2015-02-21

**This release requires PureScript v0.6.8 or later**
- Updated dependencies

## [v0.4.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.4.0) - 2015-01-10

- Updated `purescript-foldable-traversable` dependency (@garyb)
- Added `(===)` and `(/==)` combinators (@MichaelXavier)

## [v0.3.2](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.3.2) - 2014-12-02

Bump dependencies

## [v0.3.1](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.3.1) - 2014-11-26



## [v0.3.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.3.0) - 2014-11-08

Revert to pre-'machines' state. 

`purescript-strongcheck` is recommended if you need the industrial-strength version of this library, until the necessary libraries are moved into the core GitHub organization.

## [v0.2.2](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.2.2) - 2014-10-14



## [v0.2.1](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.2.1) - 2014-10-13

- Export functions of type classes, fix dependency versions, added some overlooked conveniences (@jdegoes)

## [v0.2.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.2.0) - 2014-10-10



## [v0.1.5](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.1.5) - 2014-09-26



## [0.1.4](https://github.com/purescript/purescript-quickcheck/releases/tag/0.1.4) - 2014-09-25



## [v0.1.3](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.1.3) - 2014-08-07



## [v0.1.2](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.1.2) - 2014-06-14

- Now uses "proper" `Unit` type instead of `{}` (garyb)

## [v0.1.1](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.1.1) - 2014-04-27



## [v0.1.0](https://github.com/purescript/purescript-quickcheck/releases/tag/v0.1.0) - 2014-04-27



