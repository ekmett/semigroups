0.19.1 [2019.08.27]
-------------------
* Add `GenericSemigroupMonoid`, an adapter newtype suitable for `DerivingVia`,
  to `Data.Semigroup.Generic`.
* Work around a bug related to the backported `Generic(1)` instances in this
  package (that could only be triggered on GHC 7.2 or 7.4) in which the
  hand-written `Datatype`, `Constructor`, and `Selector` instances for internal
  data types could overlap with GHC-generated instances.

0.19 [2019.05.10]
-----------------
* The `(<>)` method of the backported `Semigroup` class no longer has a default
  implementation in terms of `mappend`. This mirrors the `Data.Semigroup` API
  that was introduced in `base-4.9`. This is a breaking change for any
  `Semigroup` instances that are defined in tandem with versions of `base`
  older than 4.9.
* Make the backported `Hashable Arg` instance reflect its respective variants
  in the `hashable` package. In `hashable-1.3`, the `Hashable Arg` instance
  only hashes the first argument, lest equal values have different hashes.
* Backport the `Lift (NonEmpty a)` instance introduced in
  `template-haskell-2.15.0.0`.
* `Data.List.NonEmpty` is now unconditionally `Trustworthy`.

0.18.5 [2018.07.02]
-------------------
* Use a more efficient `sconcat` for the `Semigroup` instances for strict and
  lazy `ByteString`.

0.18.4 [2018.01.29]
-------------------
* Backport `Semigroup` instances for `Data.Ord.Down` and strict `ST`, which were
  added in `base-4.11`.

0.18.3
------
* Add `Semigroup` instance for `IO`, as well as for `Event` and `Lifetime` from
  `GHC.Event`
* Add `Eq1`, `Ord1`, `Read1`, and `Show1` instances for `NonEmpty`
* Define `Generic` and `Generic1` instances back to GHC 7.2, and expose the
  `Data.Semigroup.Generic` module on GHC 7.2

0.18.2
------
* Depend on the `bytestring-builder` package to ensure `Semigroup` instances for bytestring `Builder` and `ShortByteString` are always defined
* Allow building with `binary-0.8.3` and later

0.18.1
------
* Add the missing instance for `Data.Binary.Builder.Builder`.

0.18.0.1
--------
* Added support for `base-4.9`

0.18
--------
* Removed the partial functions `words`, `unwords`, `lines`, `unlines`

0.17.0.1
--------
* Fixed the `@since` annotations

0.17
----
* Added `groupWith`, `groupAllWith`, `groupWith1`, `groupAllWith1`
* Renamed `sortOn` to `sortWith` to match the "Comprehensive comprehensions" paper and `TransformListComp` extension.
* Add `Semigroup` instances for `Alt`, `Void`, `Proxy` and `Tagged`
* Add `Num` instances for `Min` and `Max`
* Removed `times1p` in favor of `stimes`.

0.16.2.2
--------
* Cleaned up imports to remove warnings on GHC 7.10.

0.16.2.1
--------
* Restored the ability to build on GHC < 7.6. (`Generic1` deriving was only added in GHC 7.6)

0.16.2
------
* Added `genericMappend` and supporting `GSemigroup` class for generically deriving Semigroup instances.
* Added `Arg a b` which only compares for equality/order on its first argument, which can be used to compute `argmin` and `argmax`.
* Add `Bifunctor` `Arg` instance to avoid orphans for GHC 7.10+.
* Added missing `Data.Monoid.Generic` module to source control.

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
* Nathan van Doorn fixed a number of embarrassing bugs in the `Enum` instances.

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
