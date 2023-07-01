# Alleycats

## Overview

Alleycats is a module of the [Cats](https://github.com/typelevel/cats)
project that exists to support types which are not entirely savory or
*law-abiding* but which may prove useful or necessary in some
situations.

In some cases, a type class instance can almost (but not quite) obey
the required laws (e.g. a `Monad` instance for `scala.util.Try`). In
other cases, type classes which lack laws or constraints may still be
useful in some cases (e.g. `Empty[_]`, a type class which provides
some notion of "emptiness").

Rather than argue about whether to permit these types in Cats proper, we
provide a (slightly disreputable) home for them here.

## Type classes

Alleycats introduces several new type classes. Here is an overview of
the instances introduced.

### Empty\[A\], Zero\[A\], and One\[A\]

A commonly-requested type class is one that encodes the idea of a set
having an identity element. Normally this would be done by defining a
`Monoid[A]` instance, but in some cases the idea of emptiness is
independent of a particular associative operation.

In this case, `Empty[A]` may be used in place of `Monoid[A]`. It
provides access to the *identity* element (via the `.empty` method),
and can also provide `.isEmpty` and `.nonEmpty` if an `Eq[A]` is
available.

The two other type classes, `Zero[A]` and `One[A]`, are similar,
except they correspond to `AdditiveMonoid[A]` and
`MultiplicativeMonoid[A]` (found in the `algebra.ring` package). Their
methods are called `zero` and `one`, respectively.

While none of these type classes have their own laws, they are
required not to violate the monoid laws when applicable. This means
that if `Empty[A]` and `Semigroup[A]` are both available, that
`Empty[A].empty` **must** function as an identity element for
`Semigroup[A].combine`. In fact, together these instances can be
viewed as a `Monoid[A]` (and there is an implicit method to that
effect).

The same rules apply to `Zero[A]` and `One[A]` and their respective
associative operations.

### Pure\[F\[\_\]\] and Extract\[F\[\_\]\]

The `Pure[F]` type class represents the `pure` method of
`Applicative[F]` separated from its `map` and `ap` methods. Like the
previous type classes, if `Pure[F]` and `Apply[F]` are both available
they are required to be consistent (and should provide a valid
`Applicative[F]` instance).

Similarly, `Extract[F]` represents the `extract` method of
`Comonad[F]` without `coflatMap` and `map` methods. When `Extract[F]`
and `CoflatMap[F]` are available, they should provide a valid
`Comonad[F]` instance.

### EmptyK\[F\[\_\]\]

Finally, `EmptyK[F]` generalizes the `empty[A]` method from
`MonoidK[F]`. The pattern here is the same as before --
`SemigroupK[F]` and `EmptyK[F]` should provide a valid `MonoidK[F]`
instance.

## Instances

Alleycats also provides some "disreputable" type class instances.

### Set\[\_\] instances

Scala's `Set[_]` takes advantage of the universal availability of
`.hashCode` and `.equals`. This makes it difficult to use
[parametricity](https://failex.blogspot.jp/2013/06/fake-theorems-for-free.html)
to reason about sets, and casts some doubt on their use with functors
and monads.

Alleycats provides `Monad[Set]` and `Traverse[Set]`. You can import
these instances via `import alleycats.std.set._`.

### Try\[\_\] instances

Scala's `Try[_]` is intended to replace the need for `try { ... }
catch { ... }` syntax in Scala programs, to ease error-handling, and
to transport exceptions as data. Due to the way `Try` transparently
catches exceptions in `.map` and `.flatMap`, some people are skeptical
that `Try` fulfills the necessary functor/monad laws.

Alleycats provides a `Monad[Try]`. You can import this instance via
`import alleycats.std.try._`.

### Iterable\[\_\] instances

Scala's `collection.Iterable[_]` offers no guarantees that it's immutable,
since it abstracts over the `mutable` and `immutable` variants. However it's
the type used to represent a `Map`s `values`, and its often desirable to treat the
values of a map as a `Foldable` collection. Alleycats provides a `Foldable[Iterable]`, eg:

```
import cats.syntax.all._
import alleycats.std.iterable._

// Result "AppleOrange"
Map(1 -> "Apple", 2 -> "Orange").values.combineAll
```

## Contributing

This module's goal is to be very liberal about accepting type class
instances, but to only provide instances which are absent from
Cats proper. Law-abiding instances will end up in Cats, and everything else will
end up here.
