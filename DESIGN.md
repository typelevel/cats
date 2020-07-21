# Design

The goal is to provide a lightweight, modular, and extensible library
that is approachable and powerful. We will also provide useful
documentation and examples which are type-checked by the compiler to
ensure correctness.

Cats will be designed to use modern *best practices*:

 * [simulacrum](https://github.com/typelevel/simulacrum) for minimizing type class boilerplate
 * [machinist](https://github.com/typelevel/machinist) for optimizing implicit operators
 * [scalacheck](http://scalacheck.org) for property-based testing
 * [discipline](https://github.com/typelevel/discipline) for encoding and testing laws
 * [kind-projector](https://github.com/typelevel/kind-projector) for type lambda syntax
 * [algebra](https://github.com/non/algebra) for shared algebraic structures
 * ...and of course a pure functional subset of the Scala language.

(We also plan to support [Miniboxing](http://scala-miniboxing.org) in a branch.)

Currently Cats is experimenting with providing laziness via a type
constructor (`Eval[_]`), rather than via ad-hoc by-name
parameters. This design may change if it ends up being impractical.

The goal is to make Cats as efficient as possible for both strict and
lazy evaluation. There are also issues around by-name parameters that
mean they are not well-suited to all situations where laziness is
desirable.

### Modules

Cats will be split into modules, both to keep the size of the
artifacts down and also to avoid unnecessarily tight coupling between
type classes and data types.

Cats provides the following modules:

 * `core`: Definitions for widely-used type classes and data types.
 * `laws`: The encoded laws for type classes defined in `core`, exported to assist third-party testing.
 * `kernel`: Definitions for the basic algebraic type classes 
 * `kernel-laws`: The encoded laws for type classes defined in `kernel`, exported to assist third-party testing.
 * `free`: Free structures such as the free monad, and supporting type classes.
 * `tests`: Verifies the laws, and runs any other tests. Not published.
 * `bench`: Benchmarking suites. Not published. 

As the type class families grow, it's possible that additional modules
will be added as well. Modules which depend on other libraries
(e.g. Shapeless-based type class derivation) may be added as well.
