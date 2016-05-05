## Cats

[![Build Status](https://api.travis-ci.org/typelevel/cats.svg)](https://travis-ci.org/typelevel/cats)
[![Workflow](https://badge.waffle.io/typelevel/cats.svg?label=ready&title=Ready)](https://waffle.io/typelevel/cats)
[![Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/typelevel/cats)
[![codecov.io](http://codecov.io/github/typelevel/cats/coverage.svg?branch=master)](http://codecov.io/github/typelevel/cats?branch=master)
[![Maven Central](https://img.shields.io/maven-central/v/org.typelevel/cats_2.11.svg)](https://maven-badges.herokuapp.com/maven-central/org.typelevel/cats_2.11)

### Overview

Cats is a library which provides abstractions for functional programming in Scala.

The name is a playful shortening of the word *category*.

![cats image](http://plastic-idolatry.com/erik/cats2.png)

### Getting Started

Cats is currently available for Scala 2.10 and 2.11.

To get started with SBT, simply add the following to your `build.sbt`
file:

```scala
libraryDependencies += "org.typelevel" %% "cats" % "0.5.0"
```

This will pull in all of Cats' modules. If you only require some
functionality, you can pick-and-choose from amongst these modules
(used in place of `"cats"`):

 * `cats-macros`: Macros used by Cats syntax (*required*).
 * `cats-core`: Core type classes and functionality (*required*).
 * `cats-laws`: Laws for testing type class instances.

Release notes for Cats are available in [CHANGES.md](CHANGES.md).

*Cats 0.5.0 is a pre-release: there are not currently source- or
binary-compatibility guarantees.*

### Documentation
Among the goals of Cats is to provide approachable and useful documentation.
Documentation is available in the form of tutorials on the Cats
[website](http://typelevel.org/cats), as well as through
[Scaladoc](http://typelevel.org/cats/api/#package) (also reachable through
the website).

### Building Cats

To build Cats you should have
[sbt](http://www.scala-sbt.org/0.13/tutorial/Setup.html) and [Node.js](https://nodejs.org/)
 installed. Run `sbt`, and then use any of the following commands:

 * `compile`: compile the code
 * `console`: launch a REPL
 * `test`: run the tests
 * `unidoc`: generate the documentation
 * `scalastyle`: run the style-checker on the code
 * `validate`: run tests, style-checker, and doc generation

#### Scala and Scala-js

Cats cross-compiles to both JVM and Javascript(JS). If you are not used to
working with cross-compiling builds, the first things that you will notice is that
builds:

 * Will take longer: To build JVM only, just use the `catsJVM`, or `catsJS` for
   JS only. And if you want the default project to be `catsJVM`, just copy the
   file `scripts/sbtrc-JVM` to `.sbtrc` in the root directory.

 * May run out of memory: We suggest you use
   [Paul Philips's sbt script](https://github.com/paulp/sbt-extras) that will use the settings from Cats.

### Design

The goal is to provide a lightweight, modular, and extensible library
that is approachable and powerful. We will also provide useful
documentation and examples which are type-checked by the compiler to
ensure correctness.

Cats will be designed to use modern *best practices*:

 * [simulacrum](https://github.com/mpilquist/simulacrum) for minimizing type class boilerplate
 * [machinist](https://github.com/typelevel/machinist) for optimizing implicit operators
 * [scalacheck](http://scalacheck.org) for property-based testing
 * [discipline](https://github.com/typelevel/discipline) for encoding and testing laws
 * [kind-projector](https://github.com/non/kind-projector) for type lambda syntax
 * [algebra](https://github.com/non/algebra) for shared algebraic structures
 * ...and of course a pure functional subset of the Scala language.

(We also plan to support [Miniboxing](http://scala-miniboxing.org) in a branch.)

Currently Cats is experimenting with providing laziness via a type
constructor (`Eval[_]`), rather than via ad-hoc by-name
parameters.This design may change if it ends up being impractical.

The goal is to make Cats as efficient as possible for both strict and
lazy evaluation. There are also issues around by-name parameters that
mean they are not well-suited to all situations where laziness is
desirable.

### Modules

Cats will be split into modules, both to keep the size of the
artifacts down and also to avoid unnecessarily tight coupling between
type classes and data types.

Initially Cats will support the following modules:

 * `macros`: Macro definitions needed for `core` and other projects.
 * `core`: Definitions for widely-used type classes and data types.
 * `laws`: The encoded laws for type classes, exported to assist third-party testing.
 * `tests`: Verifies the laws, and runs any other tests. Not published.

As the type class families grow, it's possible that additional modules
will be added as well. Modules which depend on other libraries
(e.g. Shapeless-based type class derivation) may be added as well.

### How can I contribute to Cats?

There are many ways to support Cats' development:

 * Fix bugs: Despite using static types, law-checking, and
   property-based testing bugs can happen. Reporting problems you
   encounter (with the documentation, code, or anything else) helps us
   to improve. Look for issues labelled "ready" as good targets, but
   **please add a comment to the issue** if you start working on one.
   We want to avoid any duplicated effort.

 * Write ScalaDoc comments: One of our goals is to have ScalaDoc
   comments for all types in Cats. The documentation should describe
   the type and give a basic usage (it may also link to relevant
   papers).

 * Write tutorials and examples: In addition to inline ScalaDoc
   comments, we hope to provide Markdown-based tutorials which can
   demonstrate how to use all the provided types. These should be
   *literate programs* i.e. narrative text interspersed with code.

 * Improve the laws and tests: Cats' type classes rely on laws (and
   law-checking) to make type classes useful and reliable. If you
   notice laws or tests which are missing (or could be improved)
   you can open an issue (or send a pull request).

 * Help with code review: Most of our design decisions are made
   through conversations on issues and pull requests. You can
   participate in these conversations to help guide the future of
   Cats.

   We will be using the **meta** label for large design decisions, and
   your input on these is especially appreciated.

 * Contribute new code: Cats is growing! If there are type classes (or
   concrete data types) which you need, feel free to contribute! You
   can open an issue to discuss your idea, or start hacking and submit
   a pull request. One advantage of opening an issue is that it may
   save you time to get other opinions on your approach.

 * Ask questions: we are hoping to make Cats (and functional
   programming in Scala) accessible to the largest number of
   people. If you have questions it is likely many other people do as
   well, and as a community this is how we can grow and improve.

### Maintainers

The current maintainers (people who can merge pull requests) are:

 * [ceedubs](https://github.com/ceedubs) Cody Allen
 * [rossabaker](https://github.com/rossabaker) Ross Baker
 * [travisbrown](https://github.com/travisbrown) Travis Brown
 * [adelbertc](https://github.com/adelbertc) Adelbert Chang
 * [tpolecat](https://github.com/tpolecat) Rob Norris
 * [stew](https://github.com/stew) Mike O'Connor
 * [non](https://github.com/non) Erik Osheim
 * [mpilquist](https://github.com/mpilquist) Michael Pilquist
 * [milessabin](https://github.com/milessabin) Miles Sabin
 * [fthomas](https://github.com/fthomas) Frank Thomas
 * [julien-truffaut](https://github.com/julien-truffaut) Julien Truffaut

We are currently following a practice of requiring at least two
sign-offs to merge PRs (and for large or contentious issues we may
wait for more). For typos or other small fixes to documentation we
relax this to a single sign-off.

### Contributing

Discussion around Cats is currently happening in the
[Gitter channel](https://gitter.im/typelevel/cats) as well as on Github
issue and PR pages. You can get an overview of who is working on what
via [Waffle.io](https://waffle.io/typelevel/cats).

Feel free to open an issue if you notice a bug, have an idea for a
feature, or have a question about the code. Pull requests are also
gladly accepted. For more information, check out the
[contributor guide](CONTRIBUTING.md). You can also see a list of past
contributors in [AUTHORS.md](AUTHORS.md).

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

### Related Projects

Cats is closely-related to [Structures](https://github.com/mpilquist/Structures);
both projects are descended from [Scalaz](https://github.com/scalaz/scalaz).

There are many related Haskell libraries, for example:

 * [semigroupoids](https://hackage.haskell.org/package/semigroupoids)
 * [profunctors](https://github.com/ekmett/profunctors)
 * [contravariant](https://github.com/ekmett/contravariant)
 * ...and so on.

### Copyright and License

All code is available to you under the MIT license, available at
http://opensource.org/licenses/mit-license.php and also in the
[COPYING](COPYING) file. The design is informed by many other
projects, in particular Scalaz.

Copyright the maintainers, 2015.
