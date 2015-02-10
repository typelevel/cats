---
layout: default
title:  "Home"
section: "home"
---

<a name="Introduction"></a>
# Introduction

Cats is an experimental library proof-of-concept library intended to provide abstractions for functional programming in the [Scala programming language](https://scala-lang.org).

<div class="msg warn">
  <p><strong>Cats is currently an experimental project under active development</strong>. Feedback and contributions are welcomed as we look to improve the project. This are evolving quickly and we currently make no guarantees about what might drastically change in the near future.</p>
</div>

There are libraries that aim to foster Functional Programming in the Scala programming language which Cats has a relationship to:

* [scalaz](https://github.com/scalaz/scalaz) The project which directly inspires Cats. Currently Cats borrows some code directly from scalaz.
* [Structures](https://github.com/mpilquist) A project very similar in nature to Cats, also derived from scalaz.
* [algebra](https://github.com/non/algebra) A project unifying algebraic type classes from Spire and Algebird. Cats depends directly on algebra for typeclasses such as Semigroup, Monoid, Eq, Order.
* [simulacrum](htts://github.com/mpilquist/simulacrum) An attempt to unify how typeclasses are represented in Scala, and which help in drastically reducing the boilerplate involved in encoding them in scala.

<a name="project-structor"></a>
# Project Structure

In an attempt to be more modular, Cats is broken up into a number of sub-projects:

* *core* - contains typeclass definitions, such as Functor, Applicative, Monad
* *data* - contains datatypes
* *laws* - laws for the typeclasses, used to validate typeclass instances
* *std* - contains typeclass instances for Scala standard library types
* *tests* - tests that check instances from *std* with laws from *laws*

<a name="getting-started"></a>
# Getting Started

Cats has not yet published artifacts, so in order to use Cats you will have to get the Cats source code, and publish jars locally, with `sbt publish-local`

Then in your project, add to your build.sbt

    libraryDependencies += "org.spire-math" %% "cats-core" % "0.1.0-SNAPSHOT"

