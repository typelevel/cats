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
 * `cats-free`: Free structures such as the free monad, and supporting type classes.
 * `tests`: Verifies the laws, and runs any other tests. Not published.

As the type class families grow, it's possible that additional modules
will be added as well. Modules which depend on other libraries
(e.g. Shapeless-based type class derivation) may be added as well.
