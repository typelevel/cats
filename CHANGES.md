## Version 0.3.0

> 2015 November 8

Version 0.3.0 is the third release of the Cats library.

This version includes new type class instances:

* [#545](https://github.com/non/cats/pull/545): `Semigroup` instances for
  `OneAnd`
* [#521](https://github.com/non/cats/pull/521): `Monoid` instances for `Xor`
  when the left side has a `Semigroup` instance and the right side has a
  `Monoid`
* [#497](https://github.com/non/cats/pull/497): `Monoid` instances for `Set`
* [#559](https://github.com/non/cats/pull/559): `Bifunctor` instances for
  `Validated`, `Ior`, `Xor`, and `XorT`
* [#569](https://github.com/non/cats/pull/569): `Functor` instances for
  `OptionT` when `F` has a `Functor` instance but not a `Monad`
* [#600](https://github.com/non/cats/pull/600): `Show` instances for `Option`
  and `OptionT`
* [#601](https://github.com/non/cats/pull/601): `Show` instances for `List`
* [#602](https://github.com/non/cats/pull/602): `Show` instances for `Set`
* [#568](https://github.com/non/cats/pull/568): Several new `Unapply` shapes

And API changes:

* [#592](https://github.com/non/cats/pull/592): `fromTryCatch` on `Xor` and
  `Validated` is now `catchOnly`
* [#553](https://github.com/non/cats/pull/553): `MonadError` now characterizes
  type constructors of kind `* -> *` instead of `(*, *) -> *`
* [#598](https://github.com/non/cats/pull/598): `OneAnd`'s type constructor type
  parameter is now before the element type
* [#610](https://github.com/non/cats/pull/610): `XorT`'s `toOption` returns an
  `OptionT[F, B]` instead of an `F[Option[B]]`
* [#518](https://github.com/non/cats/pull/518): `Free`'s `resume` method now
  returns an `Xor` instead of an `Either`
* [#575](https://github.com/non/cats/pull/575) and
  [#606](https://github.com/non/cats/pull/606): `orElse` on `Xor` and
  `Validated` does not unnecessarily constrain the type of the left side of the
  result
* [#577](https://github.com/non/cats/pull/577): `*Aux` helper classes have been
  renamed `*PartiallyApplied`

And additions:

* [#542](https://github.com/non/cats/pull/542): `WriterT`
* [#567](https://github.com/non/cats/pull/567): `Ior.fromOptions`
* [#528](https://github.com/non/cats/pull/528): `OptionT.fromOption`
* [#562](https://github.com/non/cats/pull/562): `handleErrorWith` and related
  helper methods on `MonadError`
* [#520](https://github.com/non/cats/pull/520): `toNel` and `fromList`
  conversions from `List` to `NonEmptyList`
* [#533](https://github.com/non/cats/pull/533): Conversions between types with
  `Foldable` instances and `Streaming`
* [#507](https://github.com/non/cats/pull/507): `isJvm` and `isJs` macros in the
  new `cats.macros.Platform`
* [#572](https://github.com/non/cats/pull/572): `analyze` on `FreeApplicative`
  for compilation into a `Monoid`
* [#587](https://github.com/non/cats/pull/587): Syntax for lifting values (and
  optional values) into `Validated`

And several aliases:

* [#492](https://github.com/non/cats/pull/492): `FlatMapSyntax` now includes
  `followedBy`, which is an alias for `>>`, together with a new
  `followedByEval`, which allows the caller to choose the evaluation strategy of
  the second action
* [#523](https://github.com/non/cats/pull/523): `Foldable` now has a
  `combineAll` method that aliases `fold` and allows postfix usage via
  `FoldableSyntax`

And a few removals:

* [#524](https://github.com/non/cats/pull/524): `FreeApplicative`'s redundant
  `hoist`, which was equivalent to `compile`
* [#531](https://github.com/non/cats/pull/531): `Coyoneda`'s `by`
* [#612](https://github.com/non/cats/pull/612): Many prioritization and instance
  traits are now private

And bug fixes:

* [#547](https://github.com/non/cats/pull/547): The `empty` values for
  `Monoid[Double]` and `Monoid[Float]` are now `0` instead of `1`
* [#530](https://github.com/non/cats/pull/530): `Streaming.take(n).toList` no
  longer evaluates the `n + 1`-st element
* [#538](https://github.com/non/cats/pull/538): `OneAnd`'s instances are
  properly prioritized

There are also many improvements to the documentation, tutorials, laws, tests,
and benchmarks:

* [#522](https://github.com/non/cats/pull/522): ScalaTest's `===` now uses `Eq`
  instances
* [#502](https://github.com/non/cats/pull/502): `Traverse`'s laws verify the
  consistency of `foldMap` and `traverse`
* [#519](https://github.com/non/cats/pull/519): Benchmarks (and performance
  improvements) for `Eval`
* …and many others

Thanks to everyone who filed issues, participated in the Cats Gitter channel,
submitted code, or helped review pull requests.

## Version 0.2.0

> 2015 August 31

Version 0.2.0 is the second release of the Cats library.

The most exciting feature of this release is Scala.js support, which
comes courtesy of much hard work by the Scala.js community (especially
Alistair Johnson). The SBT build configuration and project layout were
updated to support building for both the JVM and JS platforms.

Since the 0.1.2 release there was wide agreement that the split
between `cats-core` and `cats-std` was awkward. The two projects have
been combined into `cats-core`, meaning that type class instances for
common types like `List` are now available in `cats-core`.

There was also a concerted effort to improve and add documentation to
the project. Many people helped find typos, broken links, and places
where the docs could be improved. In particular, the following
tutorials were added or overhauled:

 * `Applicative`
 * `Const`
 * `Foldable`
 * `Free`
 * `FreeApplicative`
 * `Kleisli`
 * `Monad`
 * `Monoid`
 * `Semigroup`
 * `SemigroupK`
 * `Traverse`
 * `Validated`
 * `Xor`

Several new type classes and data types were introduced:

 * `Choice[F[_, _]]`
 * `Group[A]`
 * `MonadReader[F[_, _], R]`
 * `Streaming[A]` and `StreamingT[F[_], A]`
 * `Prod[F[_], G[_], A]` and `Func[F[_], A, B]`

Syntax tests were added to ensure that existing syntax worked, and
there has been some movement to enrich existing types with syntax to
make converting them to Cats types easier.

The previous `Fold[A]` type, which was used to support lazy folds, has
been replaced with `Eval[A]`. This type supports both strict and lazy
evaluation, supports lazy `map` and `flatMap`, and is trampolined for
stack safety. The definition of `Foldable#foldRight` has been updated
to something much more idiomatic and easier to reason about. The goal
is to support laziness in Cats via the `Eval[A]` type wherever
possible.

In addition to these specific changes there were numerous small bug
fixes, additions, improvements, and updates. Thanks to everyone who
filed issues, participated in the Cats Gitter channel, submitted code,
or helped review pull requests.

## Version 0.1.2

> 2015 July 17

(Due to problems with publishing 0.1.0 and 0.1.1 are incomplete.)

Version 0.1.2 is the first non-snapshot version of the Cats library!
It is intended to assist the creation of dependent libraries and to be
an early look at Cats' design.

Much of the library is quite mature, but there are no source- or
binary-compatibility guarantees at this time. The overarching design
of the library is still somewhat in flux, although mostly we expect
there will be new type classes, instances, and syntax. Some package
and module boundaries may also shift.

For complete credits, see [AUTHORS.md](AUTHORS.md) for a list of
people whose work has made this release possible.
