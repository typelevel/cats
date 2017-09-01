---
layout: home
title:  "Home"
section: "home"
---
Cats is a library which provides abstractions for functional
programming in the [Scala programming language](https://scala-lang.org).
The name is a playful shortening of the word *category*.

<div class="msg warn"> <p><strong> Cats is a new project under active
  development</strong>. Feedback and contributions are welcomed as we look
  to improve it. This project is evolving quickly and we are making no
  guarantees about stability until a 1.0 release is made.</p></div>


## <a name="getting-started" href="#getting-started"></a>Getting Started


Cats is currently available for Scala 2.10, 2.11, 2.12 and scala.js

To get started with SBT, simply add the following to your build.sbt file:

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
 
Release notes for Cats are available in [CHANGES.md](https://github.com/typelevel/cats/blob/master/CHANGES.md).


## <a name="motivations" href="#motivations"></a>Motivations


### Approachability

As this library evolves, we are placing a lot of emphasis on trying to
make decisions which will help keep this library approachable for
people new to the concepts in this library. We are collecting
anecdotes from successes and failures we have experienced in the past
in trying to teach others about these concepts, and trying to make
decisions which will help ease the process of getting acquainted to
the library for a newcomer. If you have any feedback for us in this
regard, we would love to hear from you. See the [Contributing
page](contributing.html) to find out ways to give us feedback.

### Modularity

We are trying to make the library modular. It will have a tight
core which will contain only the [type classes](typeclasses.html),
the bare minimum of data structures that are needed to support
them, and type class instances for those data structures and standard
library types.

### Documentation

We feel that having lots of documentation is a very important goal for
our project. It will be a big win towards our goal of
approachability. We will strive to have the code well documented, we
will strive to have lots of documentation external to the code, and we
will strive to have a large corpus of compiler verified examples of
how the software can be used.

Writing documentation is a huge part of developing software, and one
that is often neglected. It is also a very easy way to get started
with [contributing](contributing.html) to the project

### Efficiency

Although unfortunately there are times when programming only with
pure functions and writing efficient code in Scala can be at odds, we
are attempting to do our best at keeping our library as efficient as
we can without making unnecessary sacrifices of purity and
usability. Where sacrifices have to be made, we will strive to make
these obvious, and will keep them well documented.


## <a name="ecosystem" href="#ecosystem"></a>The cats ecosystem

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


## <a name="copyright" href="#copyright"></a>Copyright and License


All code is available to you under the MIT license, available at
http://opensource.org/licenses/mit-license.php and also in the
[COPYING](https://raw.githubusercontent.com/typelevel/cats/master/COPYING) file. The design is informed by many other
projects, in particular Scalaz.

Copyright the maintainers, 2016.
