semigroups
==========

[![Build Status](https://secure.travis-ci.org/ekmett/semigroups.png?branch=master)](http://travis-ci.org/ekmett/semigroups)

Haskellers are usually familiar with monoids. A monoid has an appending operation `<>` or `mappend` and an identity element `mempty`. A Semigroup has an append `<>`, but does not require an `mempty` element. A Monoid can be made a Semigroup with just `instance Semigroup MyMonoid`

More formally, a semigroup is an algebraic structure consisting of a set together with an associative binary operation. A semigroup generalizes a monoid in that there might not exist an identity element. It also (originally) generalized a group (a monoid with all inverses) to a type where every element did not have to have an inverse, thus the name semigroup.

Semigroups appear all over the place, except in the Haskell Prelude, so they are packaged here.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
