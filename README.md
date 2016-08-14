## Cats

[![Build Status](https://api.travis-ci.org/typelevel/cats.svg)](https://travis-ci.org/typelevel/cats)
[![Workflow](https://badge.waffle.io/typelevel/cats.svg?label=ready&title=Ready)](https://waffle.io/typelevel/cats)
[![Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/typelevel/cats)
[![codecov.io](http://codecov.io/github/typelevel/cats/coverage.svg?branch=master)](http://codecov.io/github/typelevel/cats?branch=master)
[![Maven Central](https://img.shields.io/maven-central/v/org.typelevel/cats_2.11.svg)](https://maven-badges.herokuapp.com/maven-central/org.typelevel/cats_2.11)
[![Scala.js](http://scala-js.org/assets/badges/scalajs-0.6.8.svg)](http://scala-js.org)

### Overview

Cats is a library which provides abstractions for functional programming in Scala.

The name is a playful shortening of the word *category*.

![cats image](http://plastic-idolatry.com/erik/cats2.png)

#### Why?

Scala supports both object-oriented and functional programming, and this is reflected in the hybrid approach of the
standard library. Cats augments the standard library with tools that further enable functional programming such as
`Validated`, `Monad`, and `Traverse`. A broader goal of Cats is to provide a foundation for an
[ecosystem of pure, typeful libraries](http://typelevel.org/projects/).

### Getting Started

Cats is currently available for Scala 2.10 and 2.11, and [Scala.js](http://www.scala-js.org/).

To get started with SBT, simply add the following to your `build.sbt`
file:

```scala
libraryDependencies += "org.typelevel" %% "cats" % "0.6.1"
```

This will pull in all of Cats' modules. If you only require some
functionality, you can pick-and-choose from amongst these modules
(used in place of `"cats"`):

 * `cats-macros`: Macros used by Cats syntax (*required*).
 * `cats-kernel`: Small set of basic type classes (*required*).
 * `cats-core`: Most core type classes and functionality (*required*).
 * `cats-laws`: Laws for testing type class instances.
 * `cats-free`: Free structures such as the free monad, and supporting type classes.

Release notes for Cats are available in [CHANGES.md](CHANGES.md).

*Cats is still under active development. While we don't anticipate any
 major redesigns, changes that are neither source nor binary
 compatibility are to be expected in upcoming cats releases. We will
 update the minor version of cats accordingly for such changes. Once
 cats 1.0 is released (ETA: Q3 2016), there will be an increased focus
 on making changes in compatible ways.*

### Documentation

Cats information and documentation is available on the
[website](http://typelevel.org/cats).

We also have a Scaladoc [index](http://typelevel.org/cats/api/#package).

Finally, we have a list of
[frequently-asked questions](docs/src/main/tut/faq.md).

Our goal is to have clear and comprehensive documentation. If you
notice problems, omissions, or errors, please
[let us know](CONTRIBUTING.md).

### How can I contribute to Cats?

We welcome contributions to Cats and would love for you to help build
Cats. See our [contributor guide](CONTRIBUTING.md) for more
information about how you can get involed.

### Community

Discussion around Cats is currently happening in the
[Gitter channel](https://gitter.im/typelevel/cats) as well as on Github
issue and PR pages. You can get an overview of who is working on what
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
 * [tpolecat](https://github.com/tpolecat) Rob Norris
 * [stew](https://github.com/stew) Mike O'Connor
 * [non](https://github.com/non) Erik Osheim
 * [mpilquist](https://github.com/mpilquist) Michael Pilquist
 * [milessabin](https://github.com/milessabin) Miles Sabin
 * [fthomas](https://github.com/fthomas) Frank Thomas
 * [julien-truffaut](https://github.com/julien-truffaut) Julien Truffaut
 * [kailuowang](https://github.com/kailuowang) Kailuo Wang

We are currently following a practice of requiring at least two
sign-offs to merge PRs (and for large or contentious issues we may
wait for more). For typos or other small fixes to documentation we
relax this to a single sign-off.

### Related Projects

There are many projects that integrate with Cats:

 * [Circe](https://github.com/travisbrown/circe): pure functional JSON library.
 * [Dogs](https://github.com/stew/dogs): pure functional collections and data structures.
 * [Fetch](https://github.com/47deg/fetch): efficient data access to heterogeneous data sources.
 * [FS2](https://github.com/functional-streams-for-scala): compositional, streaming I/O library
 * [Kittens](https://github.com/milessabin/kittens): automatically derived type class instances.
 * [Monix](https://github.com/monixio/monix): high-performance library for composing asynchronous and event-based programs.
 * [eff-cats](https://github.com/atnos-org/eff-cats): functional effects and effect handlers (alternative to monad transformers).

### Copyright and License

All code is available to you under the MIT license, available at
http://opensource.org/licenses/mit-license.php and also in the
[COPYING](COPYING) file. The design is informed by many other
projects, in particular [Scalaz](https://github.com/scalaz/scalaz).

Copyright the maintainers, 2015-2016.
