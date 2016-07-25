## Version 0.6.1

> 2016 July 14

Version 0.6.1 is a patch release compatible with 0.6.0.

It contains one bug fix:

* [#1062](https://github.com/typelevel/cats/pull/1173/commits/8dd682771557274a61f1e773df0f999b44a9819d): Fixed a bug in the Order and PartialOrder instances for Tuple2+ where only the first element was used in comparisons

It also contains a change to the build:

* [#1173](https://github.com/typelevel/cats/pull/1173/commits/5531d1ac7a6807c1842cd4b5b599173b14b652a9): Add binary compatibility check to all published modules

## Version 0.6.0

> 2016 May 19

Version 0.6.0 is the sixth release.

Highlights of this release:

* [#990](https://github.com/typelevel/cats/pull/990):  Separate free package into its own module
* [#1001](https://github.com/typelevel/cats/pull/1001):  Introduce cats-kernel and remove algebra dependency

This release also includes some API changes:

* [#1046](https://github.com/typelevel/cats/pull/1046):  summon `ApplicativeErrorSyntax` for `F[_]` instead of `F[_, _]`
* [#1034](https://github.com/typelevel/cats/pull/1034):  Don't combine lefts on `Xor` and `XorT` `combine`
* [#1018](https://github.com/typelevel/cats/pull/1018):  Remove blocking (JVM-only) Future instances
* [#877](https://github.com/typelevel/cats/pull/877):  Remove required laziness in Prod, fixes #615


And additions:

* [#1032](https://github.com/typelevel/cats/pull/1032):  Added `Coproduct` `fold`
* [#1028](https://github.com/typelevel/cats/pull/1028):  Added `withFilter` for `OptionT`
* [#1014](https://github.com/typelevel/cats/pull/1014):  Added `Monoid` instance for `WriterT`
* [#1029](https://github.com/typelevel/cats/pull/1029):  Added an `ApplicativeError` instance for `Kleisli` and a `MonadError[Option, Unit]` to `std.option`
* [#1023](https://github.com/typelevel/cats/pull/1023):  Add `XorT#fromEither`
* [#984](https://github.com/typelevel/cats/pull/984):  Add `Validated.ensure`
* [#1020](https://github.com/typelevel/cats/pull/1020):  Add `Traverse.traverseM`


And some code improvements:

* [#1015](https://github.com/typelevel/cats/pull/1015):  Add `Apply.map2Eval` and allow traverse laziness
* [#1024](https://github.com/typelevel/cats/pull/1024):  Override reverse on reversed `PartialOrder` to return original instance
* [#880](https://github.com/typelevel/cats/pull/880):  Optimize `Eq[Vector[A]]` instance
* [#1019](https://github.com/typelevel/cats/pull/1019):  Use `Future#successful` in `pureEval` when possible

And bug fixes:

* [#1011](https://github.com/typelevel/cats/pull/1011):  Add missing type parameters.

And some other improvements to the organization documentation, tutorials, laws and tests, including:

* [#1045](https://github.com/typelevel/cats/pull/1045):  Add a link to the `OptionT` documentation from the monad docs.
* [#1043](https://github.com/typelevel/cats/pull/1043):  Add notes about kind-projector usage in docs
* [#1042](https://github.com/typelevel/cats/pull/1042):  Cats 0.5.0 no longer pre-release
* [#1036](https://github.com/typelevel/cats/pull/1036):  Add FPiS to the "Resources for Learners" section
* [#1035](https://github.com/typelevel/cats/pull/1035):  Run kernel-law tests for JS as part of build
* [#991](https://github.com/typelevel/cats/pull/991):  Replace `~>` with `NaturalTransformation`
* [#1027](https://github.com/typelevel/cats/pull/1027):  Remove unnecessary `nelSemigroup` from `traverse` doc
* [#1022](https://github.com/typelevel/cats/pull/1022):  Add law-checking for `asMeetPartialOrder` and `asJoinPartialOrder`
* [#990](https://github.com/typelevel/cats/pull/990):  Separate free package into its own module


## Version 0.5.0

> 2016 April 28

Version 0.5.0 is the fifth release.

This release includes some API changes:

`cats.laws.discipline.eq` no longer provides `Eq` instances for `Tuple2` and `Tuple3`, these instances and together with some other new instances for `Tuple`s are now provided by `cats.std.tuple` (through inheriting the instance trait defined in algebra 0.4.2).

* [#910](https://github.com/typelevel/cats/pull/910): Remove `Streaming` and `StreamingT`
* [#967](https://github.com/typelevel/cats/pull/967): `product` and `map` can be implemented in terms of `ap`
* [#970](https://github.com/typelevel/cats/pull/970): Renamed `Kleisli#apply`to `ap`
* [#994](https://github.com/typelevel/cats/pull/994): updated to latest algebra (brought in all the new goodies)

And additions:

* [#853](https://github.com/typelevel/cats/pull/853): Adds a new `LiftTrans` type class
* [#864](https://github.com/typelevel/cats/pull/864): Add `Bifoldable`
* [#875](https://github.com/typelevel/cats/pull/875): Add `.get` method to `StateT`
* [#884](https://github.com/typelevel/cats/pull/884): Add `Applicative` syntax
* [#886](https://github.com/typelevel/cats/pull/886): Add `map` method to `OneAnd`
* [#927](https://github.com/typelevel/cats/pull/927): `XorT.ensure` method
* [#925](https://github.com/typelevel/cats/pull/925): Stack-safe `foldM`
* [#922](https://github.com/typelevel/cats/pull/922): Add `tell` and `writer` syntax for creating `Writers`.
* [#903](https://github.com/typelevel/cats/pull/903): Add `Bitraverse`
* [#928](https://github.com/typelevel/cats/pull/928): Add missing `Show` instances
* [#940](https://github.com/typelevel/cats/pull/940): More flexible `TransLift`
* [#946](https://github.com/typelevel/cats/pull/946): Added `OptionT.none`
* [#947](https://github.com/typelevel/cats/pull/947): Syntax for `ApplicativeError`
* [#971](https://github.com/typelevel/cats/pull/971): Add `toValidatedNel` to `Xor`
* [#973](https://github.com/typelevel/cats/pull/973): Add `flatMapF` for `StateT`
* [#985](https://github.com/typelevel/cats/pull/985): Add object `reducible` for reducible syntax
* [#996](https://github.com/typelevel/cats/pull/996): Add `SemigroupK` instance for `Xor`
* [#998](https://github.com/typelevel/cats/pull/998): Add `SemigroupK` instance for `Validated`
* [#986](https://github.com/typelevel/cats/pull/986): Add `Bitraverse` instances for `Validated` and `XorT`


And bug fixes:

* [#873](https://github.com/typelevel/cats/pull/873): Fix `OptionIdOps.some` to always return `Some`
* [#958](https://github.com/typelevel/cats/pull/958): Switch off scaladoc generation for Scala 2.10 due to macro problems
* [#955](https://github.com/typelevel/cats/pull/955): Rename `Id` instances to `idInstances` to make selective import easier


And removals:

* [#910](https://github.com/typelevel/cats/pull/910): Remove `Streaming` and `StreamingT`


And some other improvements to the documentation, tutorials, laws and tests, including:

* [#880](https://github.com/typelevel/cats/pull/880): Optimize `Eq[Vector[A]]` instance
* [#878](https://github.com/typelevel/cats/pull/878): Fix bug in freemonad doc
* [#870](https://github.com/typelevel/cats/pull/870): Fixed doc string for `StateT`'s `runEmptyA()`
* [#866](https://github.com/typelevel/cats/pull/866): Add some tests for `Coproduct` and `WriterT`
* [#883](https://github.com/typelevel/cats/pull/883): Delegate to `Traverse.sequence` in `Applicative.sequence`
* [#893](https://github.com/typelevel/cats/pull/893): Add `Reducible` laws
* [#923](https://github.com/typelevel/cats/pull/923): Make `Call.loop` `@tailrec` optimized
* [#916](https://github.com/typelevel/cats/pull/916): add `-P:scalajs:mapSourceURI` option
* [#909](https://github.com/typelevel/cats/pull/909): Make `Bifunctor` universal
* [#905](https://github.com/typelevel/cats/pull/905): make `Unapply` serializable
* [#902](https://github.com/typelevel/cats/pull/902): Make table in `Kleisli` readable
* [#897](https://github.com/typelevel/cats/pull/897): Add `Prod` tests
* [#938](https://github.com/typelevel/cats/pull/938): Onward to scala 2.11.8
* [#941](https://github.com/typelevel/cats/pull/941): Type class composition and `MonadState` tests
* [#949](https://github.com/typelevel/cats/pull/949): Add .ensime_cache to gitignore
* [#954](https://github.com/typelevel/cats/pull/954): Switch to use nodeJsEnv as default jsEnv to build scala.js
* [#956](https://github.com/typelevel/cats/pull/956): Upgrade scala.js from 0.6.7 -> 0.6.8
* [#960](https://github.com/typelevel/cats/pull/960): More `Reducible` tests
* [#962](https://github.com/typelevel/cats/pull/962): Improving test coverage
* [#964](https://github.com/typelevel/cats/pull/964): Clarify stabilty guarantees; drop 'proof of concept' and 'experimental'
* [#972](https://github.com/typelevel/cats/pull/972): Fix swapped f and g in `invariant` docs
* [#979](https://github.com/typelevel/cats/pull/979): Fix outdated import for `cats.syntax.apply._`
* [#995](https://github.com/typelevel/cats/pull/995): Move coverage away from bash
* [#1002](https://github.com/typelevel/cats/pull/1002): Correct the URL for *Data types à la carte*
* [#1005](https://github.com/typelevel/cats/pull/1005): fix broken link in foldable docs


As always thanks to everyone who filed issues, participated in the Cats Gitter
channel, submitted code, or helped review pull requests.



## Version 0.4.1

> 2016 February 4

Version 0.4.1 is a patch release in the 0.4 series and is binary compatible with
version 0.4.0.

This patch fixes bugs with the `dropWhile` methods on `Streaming` and
`StreamingT`.

This release corrects outdated build/POM metadata, which should fix API doc URLS.

Bug fixes:

* [#856](https://github.com/typelevel/cats/pull/856): Fix `Streaming` and `StreamingT` `dropWhile` functions

Build/publishing changes:

* [#852](https://github.com/typelevel/cats/pull/852) Update build with org change

Documentation and site improvements:

* [#859](https://github.com/typelevel/cats/pull/859) Add Contravariant documentation page
* [#861](https://github.com/typelevel/cats/pull/861) Docs: Revive useful links section. Update URLs

## Version 0.4.0

> 2016 February 1

Version 0.4.0 is the fourth release of the Cats library, and the first release
published under the `org.typelevel` group from the
[Typelevel](https://github.com/typelevel) organization on GitHub (previous
releases had been published to `org.spire-math` from `non/cats`). This means
that users will need to change the `groupId` for their Cats dependencies when
updating. If you have a line like this in your SBT build configuration, for
example:

```scala
libraryDependencies += "org.spire-math" %% "cats" % "0.3.0"
```

You will need to change it to the following:

```scala
libraryDependencies += "org.typelevel" %% "cats" % "0.4.0"
```

This release no longer includes `cats-state` or `cats-free` artifacts, since
the `cats.state` and `cats.free` packages have been moved into `cats-core`.

If you've checked out the GitHub repository locally, it would be a good idea to
update your remote to point to the new organization, which will typically look
like this (note that you should confirm that `origin` is the appropriate
remote name):

```bash
git remote set-url origin git@github.com:typelevel/cats.git
```

This release includes a large number of breaking changes, including most
prominently the introduction of a new `Cartesian` type class that is a supertype
of `Monad` (and many other types). If you use the `|@|` syntax that had
previously been provided by `Apply`, you'll need to change your imports from
`cats.syntax.apply._` to `cats.syntax.cartesian._`. For example:

```scala
scala> import cats.Eval, cats.syntax.cartesian._
import cats.Eval
import cats.syntax.cartesian._

scala> (Eval.now("v") |@| Eval.now(0.4)).tupled
res0: cats.Eval[(String, Double)] = cats.Eval$$anon$5@104f8bbd
```

Other changes in this release are described below.

This version includes API changes:

* [#555](https://github.com/typelevel/cats/pull/555): `|@|` syntax is now
  provided by `cats.syntax.cartesian`
* [#835](https://github.com/typelevel/cats/pull/835): `State` and `StateT` are
  now in the `cats.data` package
* [#781](https://github.com/typelevel/cats/pull/781): `combine` on `SemigroupK`
  is now `combineK`
* [#821](https://github.com/typelevel/cats/pull/821) and
  [#833](https://github.com/typelevel/cats/pull/833): The order of arguments for
  `ap` has been reversed (now function first)
* [#833](https://github.com/typelevel/cats/pull/833): `ap` on
  `CartesianBuilderN` is now `apWith`
* [#782](https://github.com/typelevel/cats/pull/782): `State` now uses `Eval`
  instead of `Trampoline` for stack safety
* [#697](https://github.com/typelevel/cats/pull/697): `or` for natural
  transformations is now an instance method
* [#725](https://github.com/typelevel/cats/pull/725): `orElse` on `XorT` and
  does not unnecessarily constrain the type of the left side of the result
* [#648](https://github.com/typelevel/cats/pull/648): Some types now extend
  `Product` and `Serializable` to improve type inference
* [#647](https://github.com/typelevel/cats/pull/647): `ProdInstancesN` names
  changed for consistency
* [#636](https://github.com/typelevel/cats/pull/636): `Eval` is now
  `Serializable`
* [#685](https://github.com/typelevel/cats/pull/685): Fixes for copy-paste
  errors in method names for instances for `Validated`
* [#778](https://github.com/typelevel/cats/pull/778): Unnecessary type parameter
  on `Foldable`'s `sequence_` has been removed

And additions:

* [#555](https://github.com/typelevel/cats/pull/555) and
  [#795](https://github.com/typelevel/cats/pull/795): `Cartesian`
* [#671](https://github.com/typelevel/cats/pull/671): `Coproduct` and `Inject`
* [#812](https://github.com/typelevel/cats/pull/812): `ApplicativeError`
* [#765](https://github.com/typelevel/cats/pull/765): `State` and `Free` (and
  related types) are now in the core module
* [#611](https://github.com/typelevel/cats/pull/611): `Validated` now has an
  `andThen` method that provides binding (but without the `for`-comprehension
  syntactic sugar that the name `flatMap` would bring)
* [#796](https://github.com/typelevel/cats/pull/796): `sequenceU_` and
  `traverseU_` on `Foldable`
* [#780](https://github.com/typelevel/cats/pull/780): `transformS` for `StateT`
* [#807](https://github.com/typelevel/cats/pull/807): `valueOr` for `XorT`
* [#714](https://github.com/typelevel/cats/pull/714): `orElse` for `XorT`
* [#705](https://github.com/typelevel/cats/pull/705): `getOrElseF` for `XorT`
* [#731](https://github.com/typelevel/cats/pull/731): `swap` for `Validated`
* [#571](https://github.com/typelevel/cats/pull/571): `transform` and
  `subflatMap` on `OptionT` and `XorT`
* [#757](https://github.com/typelevel/cats/pull/757) and
  [#843](https://github.com/typelevel/cats/pull/843): `compose` for
  `Alternative` and `composeK` for `MonoidK`
* [#667](https://github.com/typelevel/cats/pull/667): `OptionT.liftF`

And removals:

* [#613](https://github.com/typelevel/cats/pull/613): `Free` and
  `FreeApplicative` constructors are now private
* [#605](https://github.com/typelevel/cats/pull/605): `filter` on `Validated`
* [#698](https://github.com/typelevel/cats/pull/698): `MonadCombine` instances
  for `OptionT`
* [#635](https://github.com/typelevel/cats/pull/635): `Kleisli`'s redundant
  `lmap`, which was equivalent to `local`
* [#752](https://github.com/typelevel/cats/pull/752): `Cokleisli.cokleisli`,
  which was equivalent to `Cokleisli.apply`
* [#687](https://github.com/typelevel/cats/pull/687): Unused `XorTMonadCombine`
* [#622](https://github.com/typelevel/cats/pull/622): Many prioritization types
  are now private

And new type class instances:

* [#644](https://github.com/typelevel/cats/pull/644): `Traverse` and `Foldable`
  instances for `XorT`
* [#691](https://github.com/typelevel/cats/pull/691): Various instances for
  `Function1`
* [#628](https://github.com/typelevel/cats/pull/628) and
  [#696](https://github.com/typelevel/cats/pull/696): Various instances for
  `WriterT`
* [#673](https://github.com/typelevel/cats/pull/673): `Bifunctor` instances for
  `WriterT`
* [#715](https://github.com/typelevel/cats/pull/715) and
  [#716](https://github.com/typelevel/cats/pull/716): `Semigroup` and `Monoid`
  instances for `Validated`
* [#717](https://github.com/typelevel/cats/pull/717) and
  [#718](https://github.com/typelevel/cats/pull/718): `Semigroup` instances for
  `Xor` and `Const`
* [#818](https://github.com/typelevel/cats/pull/818): `CoflatMap` instance for
  `Vector`
* [#626](https://github.com/typelevel/cats/pull/626): `Contravariant` instances
  for `Const` and `Kleisli`
* [#621](https://github.com/typelevel/cats/pull/621): `Id` instances for
  `Kleisli`
* [#772](https://github.com/typelevel/cats/pull/772): `Reducible` instances for
  `OneAnd`
* [#816](https://github.com/typelevel/cats/pull/816): `Traverse` instances for
  `OneAnd`
* [#639](https://github.com/typelevel/cats/issues/639): `Traverse` instance
  for `Id`
* [#774](https://github.com/typelevel/cats/pull/774) and
  [#775](https://github.com/typelevel/cats/pull/775): `Show` instances for
  `Vector` and `Stream`

And bug fixes:

* [#623](https://github.com/typelevel/cats/pull/623) fixes
  [#563](https://github.com/typelevel/cats/issues/563), a bug in the behavior of
  `dropWhile_` on `Foldable`
* [#665](https://github.com/typelevel/cats/pull/665) fixes
  [#662](https://github.com/typelevel/cats/pull/662), a bug that resulted in
  re-evaluation after memoization in `Streaming`
* [#683](https://github.com/typelevel/cats/pull/683) fixes
  [#677](https://github.com/typelevel/cats/issues/677), a bug in
  `Streaming.thunk`
* [#801](https://github.com/typelevel/cats/pull/801): Fixes order effect bug in
  `foldMap` on `FreeApplicative`
* [#798](https://github.com/typelevel/cats/pull/798): Fixes bug in `filter` on
  `StreamingT`
* [#656](https://github.com/typelevel/cats/pull/656): Fixes bug in `drop` on
  `StreamingT`
* [#769](https://github.com/typelevel/cats/pull/769): Improved stack consumption
  for `Eval.Call`

And some dependency updates:

* [#833](https://github.com/typelevel/cats/pull/833): Update to Simulacrum
  0.7.0
* [#764](https://github.com/typelevel/cats/pull/764): 2.10 version is now
  2.10.6
* [#643](https://github.com/typelevel/cats/pull/643): Update to Catalysts 0.2.0
* [#727](https://github.com/typelevel/cats/pull/727): Update to Scalastyle 0.8.0

There are also many improvements to the documentation, tutorials, laws, tests,
and benchmarks, including the following:

* [#724](https://github.com/typelevel/cats/pull/724): sbt-doctest is now used to
  validate Scaladoc examples
* [#806](https://github.com/typelevel/cats/pull/806): Various improvements to
  use of Simulacrum, which is now a compile-time-only dependency
* [#734](https://github.com/typelevel/cats/pull/734): Documentation on testing
  conventions
* [#710](https://github.com/typelevel/cats/pull/710): Documentation for
  `Invariant`
* [#832](https://github.com/typelevel/cats/pull/832): Updated `Free`
  documentation
* [#824](https://github.com/typelevel/cats/pull/824): New examples for
  `Foldable`
* [#797](https://github.com/typelevel/cats/pull/797): Scaladoc examples for
  methods on `Arrow`
* [#783](https://github.com/typelevel/cats/pull/783) and others: Scaladoc
  examples for syntax methods
* [#720](https://github.com/typelevel/cats/pull/720): Expanded documentation for
  `FreeApplicative`
* [#636](https://github.com/typelevel/cats/pull/636): Law checking for `Eval`
* [#649](https://github.com/typelevel/cats/pull/649) and
  [#660](https://github.com/typelevel/cats/pull/660): Better `Arbitrary`
  instances for `Streaming` and `StreamingT`
* [#722](https://github.com/typelevel/cats/pull/722): More consistent `toString`
  for `StreamingT`
* [#672](https://github.com/typelevel/cats/pull/672): Additional laws for
  `Profunctor`
* [#668](https://github.com/typelevel/cats/pull/668),
  [#669](https://github.com/typelevel/cats/pull/669),
  [#679](https://github.com/typelevel/cats/pull/679),
  [#680](https://github.com/typelevel/cats/pull/680), and
  [#681](https://github.com/typelevel/cats/pull/681): Additional law checking
  for `Xor`, `XorT`, and `Either`
* [#707](https://github.com/typelevel/cats/pull/707): Additional testing for
  `State` and `StateT`
* [#736](https://github.com/typelevel/cats/pull/736): `map` / `flatMap`
  coherence
* [#748](https://github.com/typelevel/cats/pull/748): Left and right identity
  laws for `Kleisli`
* [#753](https://github.com/typelevel/cats/pull/753): Consistency tests for
  `Cokleisli`
* [#733](https://github.com/typelevel/cats/pull/733): Associativity laws for
  `Kleisli` and `Cokleisli` composition
* [#741](https://github.com/typelevel/cats/pull/741): Tests for
  `Unapply`-supported syntax
* [#690](https://github.com/typelevel/cats/pull/690): Error reporting
  improvements for serializability tests
* [#701](https://github.com/typelevel/cats/pull/701): Better documentation for
  the Travis CI script
* [#787](https://github.com/typelevel/cats/pull/787): Support for cross-module
  Scaladoc links

Known issues:

* [#702](https://github.com/typelevel/cats/pull/702): This change identified and
  fixed a stack safety bug in `foldMap` on `Free`, but raised other issues (see
  [#712](https://github.com/typelevel/cats/issues/712)) and was reverted in
  [#713](https://github.com/typelevel/cats/pull/713);
  [#721](https://github.com/typelevel/cats/issues/721) now tracks the non-stack
  safety of `Free`'s `foldMap`

As always thanks to everyone who filed issues, participated in the Cats Gitter
channel, submitted code, or helped review pull requests.

## Version 0.3.0

> 2015 November 8

Version 0.3.0 is the third release of the Cats library.

This version includes new type class instances:

* [#545](https://github.com/typelevel/cats/pull/545): `Semigroup` instances for
  `OneAnd`
* [#521](https://github.com/typelevel/cats/pull/521): `Monoid` instances for `Xor`
  when the left side has a `Semigroup` instance and the right side has a
  `Monoid`
* [#497](https://github.com/typelevel/cats/pull/497): `Monoid` instances for `Set`
* [#559](https://github.com/typelevel/cats/pull/559): `Bifunctor` instances for
  `Validated`, `Ior`, `Xor`, and `XorT`
* [#569](https://github.com/typelevel/cats/pull/569): `Functor` instances for
  `OptionT` when `F` has a `Functor` instance but not a `Monad`
* [#600](https://github.com/typelevel/cats/pull/600): `Show` instances for `Option`
  and `OptionT`
* [#601](https://github.com/typelevel/cats/pull/601): `Show` instances for `List`
* [#602](https://github.com/typelevel/cats/pull/602): `Show` instances for `Set`
* [#568](https://github.com/typelevel/cats/pull/568): Several new `Unapply` shapes

And API changes:

* [#592](https://github.com/typelevel/cats/pull/592): `fromTryCatch` on `Xor` and
  `Validated` is now `catchOnly`
* [#553](https://github.com/typelevel/cats/pull/553): `MonadError` now characterizes
  type constructors of kind `* -> *` instead of `(*, *) -> *`
* [#598](https://github.com/typelevel/cats/pull/598): `OneAnd`'s type constructor type
  parameter is now before the element type
* [#610](https://github.com/typelevel/cats/pull/610): `XorT`'s `toOption` returns an
  `OptionT[F, B]` instead of an `F[Option[B]]`
* [#518](https://github.com/typelevel/cats/pull/518): `Free`'s `resume` method now
  returns an `Xor` instead of an `Either`
* [#575](https://github.com/typelevel/cats/pull/575) and
  [#606](https://github.com/typelevel/cats/pull/606): `orElse` on `Xor` and
  `Validated` does not unnecessarily constrain the type of the left side of the
  result
* [#577](https://github.com/typelevel/cats/pull/577): `*Aux` helper classes have been
  renamed `*PartiallyApplied`

And additions:

* [#542](https://github.com/typelevel/cats/pull/542): `WriterT`
* [#567](https://github.com/typelevel/cats/pull/567): `Ior.fromOptions`
* [#528](https://github.com/typelevel/cats/pull/528): `OptionT.fromOption`
* [#562](https://github.com/typelevel/cats/pull/562): `handleErrorWith` and related
  helper methods on `MonadError`
* [#520](https://github.com/typelevel/cats/pull/520): `toNel` and `fromList`
  conversions from `List` to `NonEmptyList`
* [#533](https://github.com/typelevel/cats/pull/533): Conversions between types with
  `Foldable` instances and `Streaming`
* [#507](https://github.com/typelevel/cats/pull/507): `isJvm` and `isJs` macros in the
  new `cats.macros.Platform`
* [#572](https://github.com/typelevel/cats/pull/572): `analyze` on `FreeApplicative`
  for compilation into a `Monoid`
* [#587](https://github.com/typelevel/cats/pull/587): Syntax for lifting values (and
  optional values) into `Validated`

And several aliases:

* [#492](https://github.com/typelevel/cats/pull/492): `FlatMapSyntax` now includes
  `followedBy`, which is an alias for `>>`, together with a new
  `followedByEval`, which allows the caller to choose the evaluation strategy of
  the second action
* [#523](https://github.com/typelevel/cats/pull/523): `Foldable` now has a
  `combineAll` method that aliases `fold` and allows postfix usage via
  `FoldableSyntax`

And a few removals:

* [#524](https://github.com/typelevel/cats/pull/524): `FreeApplicative`'s redundant
  `hoist`, which was equivalent to `compile`
* [#531](https://github.com/typelevel/cats/pull/531): `Coyoneda`'s `by`
* [#612](https://github.com/typelevel/cats/pull/612): Many prioritization and instance
  traits are now private

And bug fixes:

* [#547](https://github.com/typelevel/cats/pull/547): The `empty` values for
  `Monoid[Double]` and `Monoid[Float]` are now `0` instead of `1`
* [#530](https://github.com/typelevel/cats/pull/530): `Streaming.take(n).toList` no
  longer evaluates the `n + 1`-st element
* [#538](https://github.com/typelevel/cats/pull/538): `OneAnd`'s instances are
  properly prioritized

There are also many improvements to the documentation, tutorials, laws, tests,
and benchmarks:

* [#522](https://github.com/typelevel/cats/pull/522): ScalaTest's `===` now uses `Eq`
  instances
* [#502](https://github.com/typelevel/cats/pull/502): `Traverse`'s laws verify the
  consistency of `foldMap` and `traverse`
* [#519](https://github.com/typelevel/cats/pull/519): Benchmarks (and performance
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
