## Cats

### Overview

Cats is a proof-of-concept library intended to provide abstractions
for functional programming in Scala.

The name is a playful shortening of the word *category*.

![cats image](http://plastic-idolatry.com/erik/cats2.png)

### Getting Started

Cats is not currently published, so you'll need to check out this
repository to try it out.

To build Cats you should have [sbt](http://www.scala-sbt.org/0.13/tutorial/Setup.html)
installed. Run `sbt`, and then use any of the following commands:

 * `compile`: compile the code
 * `console`: launch a REPL
 * `test`: run the tests
 * `unidoc`: generate the documentation

[![Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/non/cats)

[![Workflow](https://badge.waffle.io/non/cats.png?label=ready&title=Ready)](https://waffle.io/non/cats)

### Design

The goal is to provide a lightweight, modular, and extensible library
that is approachable and powerful. We will also provide useful
documentation and examples which are type-checked by the compiler to
ensure correctness.

Cats will be designed to use modern *best practices*:

 * [simulacrum](https://github.com/mpilquist/simulacrum) for minimizing typeclass boilerplate
 * [machinist](https://github.com/typelevel/machinist) for optimizing implicit operators
 * [scalacheck](http://scalacheck.org) for property-based testing
 * [discipline](https://github.com/typelevel/discipline) for encoding and testing laws
 * [kind-projector](https://github.com/non/kind-projector) for type lambda syntax
 * [algebra](https://github.com/non/algebra) for shared algebraic structures
 * ...and of course a pure functional subset of the Scala language.

(We also plan to support [Miniboxing](http://scala-miniboxing.org) in a branch.)

Currently Cats is experimenting with providing laziness via a type
constructor (`Lazy[_]`), rather than via ad-hoc by-name
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

 * `core`: Almost exclusively definitions for widely-used type classes.
 * `std`: Standard type class instances and other useful data types.
 * `data`: Data types and corresponding type class instances.
 * `laws`: The encoded laws for type classes, exported to assist third-party testing.
 * `tests`: Verifies the laws, and runs any other tests. Not published.

As the type class families grow, it's possible that additional modules
will be added as well. Modules which depend on other libraries
(e.g. Shapeless-based type class derivation) may be added as well.

### How can I contribute to Cats?

There are many ways to support Cats' development:

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

 * Fix bugs: Despite using static types, law-checking, and
   property-based testing bugs can happen. Reporting problems you
   encounter (with the documentation, code, or anything else) helps us
   to improve.
   
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
 * [non](https://github.com/non) Erik Osheim
 * [mpilquist](https://github.com/mpilquist) Michael Pilquist
 * [stew](https://github.com/stew) Mike O'Connor
 * [milessabin](https://github.com/milessabin) Miles Sabin
 * [tpolecat](https://github.com/tpolecat) Rob Norris
 * [travisbrown](https://github.com/travisbrown) Travis Brown
 
We are currently following a practice of requiring at least two
sign-offs to merge PRs (and for large or contentious issues we may
wait for more).

### Contributing

Discussion around Cats is currently happening in the
[Gitter channel](https://gitter.im/non/cats) as well as on Github
issue and PR pages. You can get an overview of who is working on what
via [Waffle.io](https://waffle.io/non/cats).

Feel free to open an issue if you notice a bug, have an idea for a
feature, or have a question about the code. Pull requests are also
gladly accepted.

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
