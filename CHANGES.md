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
