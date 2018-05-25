# Contributor guide

## About this document

This guide is for people who would like to be involved in building Cats.

This guide assumes that you have some experience doing Scala
development. If you get stuck on any of these steps, please feel free
to [ask for help](#getting-in-touch).

## How can I help?

Cats follows a standard
[fork and pull](https://help.github.com/articles/using-pull-requests/)
model for contributions via GitHub pull requests.

Below is a list of the steps that might be involved in an ideal
contribution. If you don't have the time to go through every step,
contribute what you can, and someone else will probably be happy to
follow up with any polishing that may need to be done.

If you want to touch up some documentation or fix typos, feel free to
skip these steps and jump straight to submitting a pull request.

 1. [Find something that belongs in cats](#find-something-that-belongs-in-cats)
 2. [Let us know you are working on it](#let-us-know-you-are-working-on-it)
 3. [Build the project](#build-project)
 4. [Implement your contribution](#write-code)
 5. [Write tests](#write-tests)
 6. [Write documentation](#write-documentation)
 7. [Write examples](#write-examples)
 8. [Submit pull request](#submit-a-pull-request)

### Find something that belongs in cats

Looking for a way that you can help out? Check out our [open issues](https://github.com/typelevel/cats/issues) and look for ones tagged as _**help wanted**_ or _**low-hanging fruit**_. These issues are the easiest way to start contributing, but if you find other items that catch your eye, you're most than welcome to tackle them!

Make sure that it's not already assigned to someone and that nobody has left a comment saying that they are working on it!

(Of course, you can also comment on an issue someone is already
working on and offer to collaborate.)

Have an idea for something new? That's great! We recommend that you
make sure it belongs in cats before you put effort into creating a
pull request. The preferred ways to do that are to either:

 * [create a GitHub issue](https://github.com/typelevel/cats/issues/new)
   describing your idea.
 * get feedback in the [cats Gitter room](https://gitter.im/typelevel/cats).

Things that belong in cats generally have the following characteristics:

 * Their behavior is governed by well-defined [laws](laws).
 * They provide general abstractions.

Laws help keep types consistent, and remove ambiguity or sensitivity
about how particular instances can behave. We've found that types with
laws are often more useful than *lawless* types

(In some cases, *lawless* type classes and instances are useful. We
intend to support some of these in a future module.)

By staying general, Cats' abstractions are widely-applicable, and not
tied to particular libraries or strategies. Rather than being a
library to work with databases, HTTP requests, etc, Cats provides
abstractions used to build those libraries.

Cats (and especially `cats-core`) is intended to be lean and modular.
Some great ideas are not a great fit, either due to their size or
their complexity. In these cases, creating your own library that
depends on Cats is probably the best plan.

#### Cats subprojects

Cats has other _companion_ projects, described next:

* [cats-effect](https://github.com/typelevel/cats-effect): a project aimed to provide a standard IO type for the Cats ecosystem, as well as a set of typeclasses (and associated laws) which characterize general effect types.
* [cats-mtl](https://github.com/typelevel/cats-mtl): provides transformer typeclasses for cats' Monads, Applicatives and Functors.
* [mouse](https://github.com/typelevel/mouse): a small companion to the Cats functional programming library for Scala. It includes convenience extension methods for Scala standard library classes, including some found in scalaz that are not in Cats.


### Let us know you are working on it

If there is already a GitHub issue for the task you are working on,
leave a comment to let people know that you are working on it. If
there isn't already an issue and it is a non-trivial task, it's a good
idea to create one (and note that you're working on it). This prevents
contributors from duplicating effort.

### Build the project

First you'll need to checkout a local copy of the code base:

```sh
git clone git@github.com:typelevel/cats.git
```

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

 * Will take longer: To build JVM only, just use the `catsJVM`, or `catsJS`
   for JS only. And if you want the default project to be `catsJVM`, just copy the
   file `scripts/sbtrc-JVM` to `.sbtrc` in the root directory.

 * May run out of memory: We suggest you use
   [Paul Philips's sbt script](https://github.com/paulp/sbt-extras) that will use the settings from Cats.

### Write code

[See guidelines](/cats/guidelines.html).

### Attributions

If your contribution has been derived from or inspired by other work, please
state this in its ScalaDoc comment and provide proper attribution. When
possible, include the original authors' names and a link to the original work.

### Write tests

- Tests for cats-core go into the tests module, under the `cats.tests` package.
- Tests for additional modules, such as 'jvm', go into the tests directory within that module.
- Cats tests should extend `CatsSuite`.  `CatsSuite` integrates [ScalaTest](http://www.scalatest.org/)
with [Discipline](https://github.com/typelevel/discipline) for law checking, and imports all syntax and standard instances for convenience.
- The first parameter to the `checkAll` method provided by
 [Discipline](https://github.com/typelevel/discipline), is the name of the test and will be output to the
 console as part of the test execution. By convention:
 - When checking laws, this parameter generally takes a form that describes the data type being tested.
 For example the name *"Validated[String, Int]"* might be used when testing a type class instance
 that the `Validated` data type supports.
 - An exception to this is serializability tests, where the type class name is also included in the name.
 For example, in the case of `Validated`, the serializability test would take the form,
 *"Applicative[Validated[String, Int]"*, to indicate that this test is verifying that the `Applicative`
 type class instance for the `Validated` data type is serializable.
 - This convention helps to ensure clear and easy to understand output, with minimal duplication in the output.
- It is also a goal that, for every combination of data type and supported type class instance:
 - Appropriate law checks for that combination are included to ensure that the instance meets the laws for that type class.
 - A serializability test for that combination is also included, such that we know that frameworks which
 rely heavily on serialization, such as `Spark`, will have strong compatibility with `cats`.
 - Note that custom serialization tests are not required for instances of type classes which come from
 `algebra`, such as `Monoid`, because the `algebra` laws include a test for serialization.
- For testing your laws, it is advised to check [this guide](https://typelevel.org/cats/typeclasses/lawtesting.html).

### Binary compatibility

It is important to verify that the feature you are implementing is compatible with Scala 2.11.x and Scala 2.12.x (Scala <2.10.x is not supported). When you submit a PR, Travis makes this check, but it is time-expensive, so you can assure this step beforehand by issuing the command `+2.11.12`, which sets the cats' Scala version to `2.11.12` and then run `mimaReportBinaryIssues`.

A summary of these steps is as follows:

```
$ sbt
> +2.11.12
> mimaReportBinaryIssues
```
This procedure will report if there are any binary compatibility issues that should be fixed.

As a side note, the latter command uses [sbt-mima](https://github.com/lightbend/migration-manager) (shorthand for "Migration Manager") plugin and you can find more information about it [here](https://github.com/lightbend/migration-manager/wiki).


## Contributing documentation

### source for the documentation
The documentation for this website is stored alongside the source, in the [docs subproject](https://github.com/typelevel/cats/tree/master/docs).

* The source for the tut compiled pages is in `docs/src/main/tut`
* The menu structure for these pages is in `docs/src/main/resources/microsite/data/menu.yml`

### Generating the Site

run `sbt docs/makeMicrosite`

### Previewing the site

1. Install jekyll locally. Depending on your platform, you might do this with:

    `yum install jekyll`

    `apt-get install ruby-full; gem install jekyll`

    `gem install jekyll`

2. In a shell, navigate to the generated site directory in `docs/target/site`

3. Start jekyll with `jekyll serve`

4. Navigate to http://localhost:4000/cats/ in your browser

5. Make changes to your site, and run `sbt docs/makeMicrosite` to regenerate the site. The changes should be reflected as soon as you run `makeMicrosite`.

### Compiler verified documentation

We use [tut](https://github.com/tpolecat/tut) to compile source code
which appears in the documentation, this ensures us that our examples
should always compile, and our documentation has a better chance of
staying up-to-date.

### Write examples

One of the best ways to provide examples is doctest, here is [an example](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/Functor.scala#L19-L33). Doctest is a [sbt plugin](https://github.com/tkawachi/sbt-doctest) which generates tests based on the syntax mentioned above and runs when sbt's `test` task is invoked. You can find more information in the plugin documentation.

### Submit a pull request

Before you open a pull request, you should make sure that `sbt
validate` runs successfully. Travis will run this as well, but it may
save you some time to be alerted to style problems earlier.

If your pull request addresses an existing issue, please tag that
issue number in the body of your pull request or commit message. For
example, if your pull request addresses issue number 52, please
include "fixes #52".

If you make changes after you have opened your pull request, please
add them as separate commits and avoid squashing or
rebasing. Squashing and rebasing can lead to a tidier git history, but
they can also be a hassle if somebody else has done work based on your
branch.

## How did we do?

Getting involved in an open source project can be tough. As a
newcomer, you may not be familiar with coding style conventions,
project layout, release cycles, etc. This document seeks to demystify
the contribution process for the cats project.

It may take a while to familiarize yourself with this document, but if
we are doing our job right, you shouldn't have to spend months poring
over the project source code or lurking the
[Gitter room](https://gitter.im/typelevel/cats) before you feel comfortable
contributing. In fact, if you encounter any confusion or frustration
during the contribution process, please create a GitHub issue and
we'll do our best to improve the process.

## Getting in touch

Discussion around Cats is currently happening in the
[Gitter channel](https://gitter.im/typelevel/cats) as well as on Github
issue and PR pages. You can get an overview of who is working on what
via [Waffle.io](https://waffle.io/typelevel/cats).

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
contact one of the [project maintainers](https://github.com/typelevel/cats#maintainers)
in private. We hope to avoid letting minor aggressions and misunderstandings
escalate into larger problems.

If you are being harassed, please contact one of [us](https://github.com/typelevel/cats#maintainers)
immediately so that we can support you.
