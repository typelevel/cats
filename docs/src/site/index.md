---
layout: default
title:  "Home"
section: "home"
---
Cats is a library which provides abstractions for functional
programming in the [Scala programming language](https://scala-lang.org).
The name is a playful shortening of the word *category*.

<div class="msg warn"> <p><strong> Cats is a new project under active
  development</strong>. Feedback and contributions are welcomed as we look
  to improve it. This project is evolving quickly and we are making no
  guarantees about stability until a 1.0 release is made (current est.
  around Q3 2016).</p> </div>


<a name="getting-started"></a>
# Getting Started

Cats is currently available for Scala 2.10 and 2.11.

To get started with SBT, simply add the following to your build.sbt file:

    libraryDependencies += "org.typelevel" %% "cats" % "0.6.1"

This will pull in all of Cats' modules. If you only require some
functionality, you can pick-and-choose from amongst these modules
(used in place of `"cats"`):

 * `cats-macros`: Macros used by Cats syntax (*required*).
 * `cats-kernel`: Small set of basic type classes (*required*).
 * `cats-core`: Most core type classes and functionality (*required*).
 * `cats-laws`: Laws for testing type class instances.
 * `cats-free`: Free structures such as the free monad, and supporting type classes.

Release notes for Cats are available in [CHANGES.md](https://github.com/typelevel/cats/blob/master/CHANGES.md).

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

<a name="project-structure"></a>
# Project Structure

In an attempt to be more modular, Cats is broken up into a number of sub-projects:

* *core* - contains type class definitions (e.g. Functor, Applicative, Monad), essential datatypes, and
  type class instances for those datatypes and standard library types
* *laws* - laws for the type classes, used to validate type class instances
* *cats-free* - free structures such as the free monad, and supporting type classes.
* *tests* - tests that check type class instances with laws from *laws*
* *docs* - The source for this website

<a name="copyright"></a>
# Copyright and License

All code is available to you under the MIT license, available at
http://opensource.org/licenses/mit-license.php and also in the
[COPYING](https://raw.githubusercontent.com/typelevel/cats/master/COPYING) file. The design is informed by many other
projects, in particular Scalaz.

Copyright the maintainers, 2016.
