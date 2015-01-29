## Cats

![cats image](http://plastic-idolatry.com/erik/cats.png)

### Overview

Cats is a proof-of-concept library intended to provide abstractions
for functional programming in Scala.

The name is a playful shortening of the word *category*.

### Getting Started

Cats is not currently published, so you'll need to check out this
repository to try it out.

To build Cats you should have [sbt](http://www.scala-sbt.org/0.13/tutorial/Setup.html)
installed. Run `sbt`, and then use any of the following commands:

 * `compile`: compile the code.
 * `console`: launch a REPL
 * `test`: run the tests

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/non/cats)

### Design

The goal is to provide a lightweight, modular, and extensible library
that is approachable and powerful. The library will use modern *best
practices*:

 * [simulacum](https://github.com/mpilquist/simulacrum) for minimizing typeclass boilerplate
 * [machinist](https://github.com/typelevel/machinist) for optimizing implicit operators
 * [scalacheck](http://scalacheck.org) for property-based testing
 * [discipline](https://github.com/typelevel/discipline) for encoding and testing laws
 * [kind-projector](https://github.com/non/kind-projector) for type lambda syntax
 * [algebra](https://github.com/non/algebra) for shared algebraic structures
 * ...and of course a pure functional subset of the Scala language.

Another design goal is to support [Miniboxing](http://scala-miniboxing.org) in a fork.

Currently Cats is experimenting with providing laziness via a type
constructor, rather than via ad-hoc by-name parameters. This design
may change if it ends up being impractical.

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
 * `laws`: The encoded laws for type classes, exported to assist third-party testing.
 * `tests`: Verifies the laws, and runs any other tests. Not published.

As the type class families grow, it's possible that additional modules
will be added as well. Modules which depend on other libraries
(e.g. Shapeless-based type class derivation) may be added as well.

### Contributing

Discussion around Cats is currently happening in the [Gitter channel](https://gitter.im/non/cats).

Feel free to open an issue if you notice a bug, have an idea for a
feature, or have a question about the code. Pull requests are also
gladly accepted.

People are expected to follow the [Typelevel Code of Conduct](http://typelevel.org/conduct.html) when
discussing Cats on the Github page, Gitter channel, or other venues.

Concerns or issues can be sent to Erik Osheim (*erik@osheim.org*).

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
http://opensource.org/licenses/mit-license.php and also in the COPYING
file. The design is informed by many other projects, in particular
Scalaz.

Copyright Erik Osheim, 2015.
