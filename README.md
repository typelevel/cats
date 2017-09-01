## Cats

[![Build Status](https://api.travis-ci.org/typelevel/cats.svg)](https://travis-ci.org/typelevel/cats)
[![Workflow](https://badge.waffle.io/typelevel/cats.svg?label=ready&title=Ready)](https://waffle.io/typelevel/cats)
[![Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/typelevel/cats)
[![codecov.io](http://codecov.io/github/typelevel/cats/coverage.svg?branch=master)](http://codecov.io/github/typelevel/cats?branch=master)
[![Latest version](https://index.scala-lang.org/typelevel/cats/cats-core/latest.svg?color=orange&v=1)](https://index.scala-lang.org/typelevel/cats/cats-core)
[![Scala.js](http://scala-js.org/assets/badges/scalajs-0.6.14.svg)](http://scala-js.org)

### Overview

Cats is a library which provides abstractions for functional programming in Scala.

The name is a playful shortening of the word *category*.

![cats image](http://plastic-idolatry.com/erik/cats2.png)

#### Why?

Scala supports both object-oriented and functional programming, and this is reflected in the hybrid approach of the
standard library. Cats augments the standard library with tools that further enable functional programming such as
`Validated`, `Monad`, and `Traverse`. A broader goal of Cats is to provide a foundation for an
[ecosystem of pure, typeful libraries](https://github.com/typelevel/cats#the-cats-ecosystem).

### Getting Started

Cats is currently available for Scala 2.10, 2.11 and 2.12, and [Scala.js](http://www.scala-js.org/).

To get started with SBT, simply add the following to your `build.sbt`
file:

```scala
libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-MF"
```

This will pull in the cats-core module. If you require some other
functionality, you can pick-and-choose from amongst these modules
(used in place of `"cats-core"`):

 * `cats-macros`: Macros used by Cats syntax (*required*).
 * `cats-kernel`: Small set of basic type classes (*required*).
 * `cats-core`: Most core type classes and functionality (*required*).
 * `cats-laws`: Laws for testing type class instances.
 * `cats-free`: Free structures such as the free monad, and supporting type classes.
 * `cats-testkit`: lib for writing tests for type class instances using laws.
 
 There are several other cats modules that are in separate repos so that they can 
 maintain independent release cycles. 
 
 * [`cats-effect`](https://github.com/typelevel/cats-effect): standard `IO` type together with `Sync`, `Async` and `Effect` type classes 
 * [`cats-mtl`](https://github.com/typelevel/cats-mtl): transformer typeclasses for cats' Monads, Applicatives and Functors.
 * [`alleycats`](https://github.com/non/alleycats): cats instances and classes which are not lawful.
 * [`mouse`](https://github.com/benhutchison/mouse): a small companion to cats that provides convenient syntax (aka extension methods) 
 

Release notes for Cats are available in [CHANGES.md](CHANGES.md).

*Cats is still under active development. While we don't anticipate any
 major redesigns, changes that are neither source nor binary
 compatible are to be expected in upcoming cats releases. We will
 update the minor version of cats accordingly for such changes. Once
 cats 1.0 is released (ETA: Q4 2017), there will be an increased focus
 on making changes in compatible ways.*

#### Enhancing type inference

To use cats you'll need sometimes support for improved type inference. To enable it for any supported Scalac version, use this [sbt plugin](https://github.com/fiadliel/sbt-partial-unification#sbt-partial-unification).

### Documentation

Cats information and documentation is available on the
[website](http://typelevel.org/cats).

We also have a Scaladoc [index](http://typelevel.org/cats/api/#package).

Finally, we have a list of
[frequently-asked questions](docs/src/main/tut/faq.md).

Our goal is to have clear and comprehensive documentation. If you
notice problems, omissions, or errors, please
[let us know](CONTRIBUTING.md).

### The cats ecosystem

Many projects integrate with cats. By sharing the same set of
type classes, instances and data types, projects can speak the same "cats
language", and integrate with each other with ease.

#### General purpose libraries to support pure functional programming

 * [Dogs](https://github.com/stew/dogs): pure functional collections and data structures.
 * [Kittens](https://github.com/milessabin/kittens): automatic type class derivation for Cats and generic utility functions
 * [eff](https://github.com/atnos-org/eff): functional effects and effect handlers (alternative to monad transformers).
 * [Freestyle](https://github.com/47deg/freestyle): pure functional framework for Free and Tagless Final apps & libs.
 * [mainecoon](https://github.com/kailuowang/mainecoon): Transform and compose tagless final encoded algebras
 * [iota](https://github.com/frees-io/iota): Fast [co]product types with a clean syntax
 * [origami](https://github.com/atnos-org/origami): monadic folds

#### Libraries with more specific uses

 * [Circe](https://github.com/circe/circe): pure functional JSON library.
 * [Fetch](https://github.com/47deg/fetch): efficient data access to heterogeneous data sources.
 * [Frameless](https://github.com/typelevel/frameless): Expressive types for Spark.
 * [FS2](https://github.com/functional-streams-for-scala): compositional, streaming I/O library
 * [doobie](https://github.com/tpolecat/doobie): a pure functional JDBC layer for Scala
 * [Monix](https://github.com/monix/monix): high-performance library for composing asynchronous and event-based programs.
 * [http4s](https://github.com/http4s/http4s): A minimal, idiomatic Scala interface for HTTP
 * [hammock](https://github.com/pepegar/hammock): Purely functional HTTP client
 * [atto](https://github.com/tpolecat/atto): friendly little text parsers
 * [decline](https://github.com/bkirwi/decline): A composable command-line parser
 * [seals](https://github.com/durban/seals): tools for schema evolution and language-integrated schemata
 * [grafter](https://github.com/zalando/grafter): dependency-injection library using the `Reader` pattern
 * [finch](https://github.com/finagle/finch): Scala combinator library for building Finagle HTTP services 
 * [pureconfig](https://github.com/pureconfig/pureconfig): A boilerplate-free library for loading configuration files
 

*Feel free to submit a PR if you want a project you maintain to be added to this list.*


### How can I contribute to Cats?

We welcome contributions to Cats and would love for you to help build
Cats. See our [contributor guide](CONTRIBUTING.md) for more
information about how you can get involved.

### Community

Discussion around Cats is currently happening on Github issue and PR pages
as well as in two Gitter channels: 

[Gitter channel cats](https://gitter.im/typelevel/cats) is for general user 
questions and discussions, and 

[Gitter channel cats-dev](https://gitter.im/typelevel/cats-dev)
is dedicated for cats development related discussions. For people who wants to 
follow closely and/or to participate in the decisions in cats development, 
this is the room to join. 

You can get an overview of who is working on what
via [Waffle.io](https://waffle.io/typelevel/cats).

People are expected to follow the
[Typelevel Code of Conduct](http://typelevel.org/conduct.html) when
discussing Cats on the Github page, Gitter channel, or other
venues.

We hope that our community will be respectful, helpful, and kind. If
you find yourself embroiled in a situation that becomes heated, or
that fails to live up to our expectations, you should disengage and
contact one of the [project maintainers](#maintainers) in private. We
hope to avoid letting minor aggressions and misunderstandings escalate
into larger problems.

If you are being harassed, please contact one of [us](#maintainers)
immediately so that we can support you.

### Maintainers

The current maintainers (people who can merge pull requests) are:

 * [ceedubs](https://github.com/ceedubs) Cody Allen
 * [rossabaker](https://github.com/rossabaker) Ross Baker
 * [johnynek](https://github.com/johnynek) P. Oscar Boykin
 * [travisbrown](https://github.com/travisbrown) Travis Brown
 * [adelbertc](https://github.com/adelbertc) Adelbert Chang
 * [peterneyens](https://github.com/peterneyens) Peter Neyens
 * [edmundnoble](https://github.com/edmundnoble) Edmund Noble
 * [tpolecat](https://github.com/tpolecat) Rob Norris
 * [stew](https://github.com/stew) Mike O'Connor
 * [non](https://github.com/non) Erik Osheim
 * [mpilquist](https://github.com/mpilquist) Michael Pilquist
 * [milessabin](https://github.com/milessabin) Miles Sabin
 * [djspiewak](https://github.com/djspiewak) Daniel Spiewak
 * [fthomas](https://github.com/fthomas) Frank Thomas
 * [julien-truffaut](https://github.com/julien-truffaut) Julien Truffaut
 * [kailuowang](https://github.com/kailuowang) Kailuo Wang

We are currently following a practice of requiring at least two
sign-offs to merge PRs (and for large or contentious issues we may
wait for more). For typos or other small fixes to documentation we
relax this to a single sign-off.


### Copyright and License

All code is available to you under the MIT license, available at
http://opensource.org/licenses/mit-license.php and also in the
[COPYING](COPYING) file. The design is informed by many other
projects, in particular [Scalaz](https://github.com/scalaz/scalaz).

Copyright the maintainers, 2015-2016.
