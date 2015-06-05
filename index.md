---
layout: default
title:  "Home"
section: "home"
---
Cats is an experimental library intended to provide abstractions for
functional programming in the
[Scala programming language](https://scala-lang.org). The name is a
playful shortening of the word *category*.

<div class="msg warn"> <p><strong> Cats is currently an experimental
  project under active development</strong>. Feedback and
  contributions are welcomed as we look to improve the project. This
  are evolving quickly and we currently make no guarantees about what
  might drastically change in the near future.</p> </div>


<a name="getting-started"></a>
# Getting Started

Cats has not yet published artifacts, so in order to use Cats you will have to get the Cats source code, and publish jars locally, with `sbt publish-local`

Then in your project, add to your build.sbt

    libraryDependencies += "org.spire-math" %% "cats-core" % "0.1.0-SNAPSHOT"

<a name "motivations"></a>
# Motivations

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
core which will contain only the [typeclasses](_tut/typeclasses.html) and
the bare minimum of data structures that are needed to support
them. Support for using these typeclasses with the Scala standard library
will be in the `std` project. 

### Documentation

We feel that having lots of documentation is a very important goal for
our project. It will be a big win towards our goal of
approachability. We will strive to have the code well documented, we
will strive to have lots of documentation external to the code, and We
will strive to have a large corpus of compiler verified examples of
how the software can be used.

Writing documentation is a huge part of developing software, and one
that is often neglected. It is also a very easy way to get started
with [contributing](contributing.html) to the project

### Efficiency

Although, unfortunately there are times when programming only with
pure functions and writing efficient code in Scala can be at odds, we
are attempting to do our best at keeping our library as efficient as
we can without making unnecessary sacrifices of purity and
usability. Where sacrifices have to be made, we will strive to make
these obvious, and will keep the well documented.

<a name="project-structure"></a>
# Project Structure

In an attempt to be more modular, Cats is broken up into a number of sub-projects:

* *core* - contains typeclass definitions, such as Functor, Applicative, Monad and essential datatypes
* *std* - contains typeclass instances for Scala standard library types
* *laws* - laws for the typeclasses, used to validate typeclass instances
* *tests* - tests that check instances from *std* with laws from *laws*
* *docs* - The source for this website

<a name="copyright"></a>
# Copyright and License

All code is available to you under the MIT license, available at
http://opensource.org/licenses/mit-license.php and also in the
[COPYING](https://raw.githubusercontent.com/non/cats/master/COPYING) file. The design is informed by many other
projects, in particular Scalaz.

Copyright the maintainers, 2015.
