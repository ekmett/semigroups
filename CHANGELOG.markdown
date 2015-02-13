0.16.2
------
* Added `genericMappend` and supporting `GSemigroup` class for generically deriving Semigroup instances.

0.16.1
------
* Added `Semigroup` instances for various Builder constructions in `text` and `bytestring` where available.
* Added `MonadFix` and `MonadPlus` instances for `NonEmpty`.

0.16.0.1
--------
* Bumped `deepseq` version bound for GHC 7.10 compatibility.

0.16
----
* `times1p` and `timesN` are now reduced to accepting only a `Natural` argument. `Whole` doesn't exist in GHC 7.10's Numeric.Natural, and `nats` version 1 has removed support for the class.

0.15.4
------
* Use `Data.Coerce.coerce` on GHC 7.8+ to reduce the number of eta-expansions in the resulting core.
* Avoid conflict with pending `Foldable.length` in base.

0.15.3
------
* `instance NFData a => NFData (NonEmpty a)`
* Added `NFData` instances for the types in Data.Semigroup

0.15.2
------
* Fixed a Trustworthiness problem for GHC 7.8+

0.15.1
------
* Nathan van Doorn fixed a number of embarassing bugs in the `Enum` instances.

0.15
----
* `instance IsList NonEmpty`

0.14
----
* Allow for manual removal of dependencies to support advanced sandbox users who explicitly want to avoid compiling certain dependencies
  they know they aren't using.

  We will fix bugs caused by any combination of these package flags, but the API of the package should be considered the default build
  configuration with all of the package dependency flags enabled.

* Will now build as full-fledged `Safe` Haskell if you configure with -f-hashable.

* Added some missing `Generic`/`Generic`/`Hashable` instances

0.13.0.1
--------
* `Generic` support requires `ghc-prim` on GHC 7.4.

0.13
----
* Added instances for 'Generic', 'Foldable', 'Traversable', 'Enum', 'Functor', 'Hashable', 'Applicative', 'Monad' and 'MonadFix'

0.12.2
------
* Vastly widened the dependency bound on `text` and `bytestring`.

0.12.1
-------
* Updated to support the new version of `text`.
* Added `transpose`, `sortBy` and `sortWith`.

0.12
----
* Added an instance for `Const r`.
* Added `some1`

0.11
----
* Added the missing instance for `HashSet`.

0.10
----
* Added support for `unordered-containers`, `bytestring` and `text`.

0.9.2
-----
* Added a `DefaultSignature` for `(<>)` in terms of `mappend`.


0.9.1
-----
* Added `timesN`.

0.9
---
* Moved `Numeric.Natural` to a separate `nats` package.
