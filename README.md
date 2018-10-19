## Cats
![cats image](http://plastic-idolatry.com/erik/cats2.png)

[![Build Status](https://api.travis-ci.org/typelevel/cats.svg)](https://travis-ci.org/typelevel/cats)
[![Workflow](https://badge.waffle.io/typelevel/cats.svg?label=ready&title=Ready)](https://waffle.io/typelevel/cats)
[![Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/typelevel/cats)
[![codecov.io](http://codecov.io/github/typelevel/cats/coverage.svg?branch=master)](http://codecov.io/github/typelevel/cats?branch=master)
[![Latest version](https://index.scala-lang.org/typelevel/cats/cats-core/latest.svg?color=orange&v=1)](https://index.scala-lang.org/typelevel/cats/cats-core)
[![Scala.js](http://scala-js.org/assets/badges/scalajs-0.6.14.svg)](http://scala-js.org)

### Overview

Cats is a library which provides abstractions for functional programming in the [Scala programming language](https://scala-lang.org).
The name is a playful shortening of the word *category*.

Scala supports both object-oriented and functional programming, and this is reflected in the hybrid approach of the
standard library. Cats strives to provide functional programming abstractions that are core, [binary compatible](http://typelevel.org/cats/#binary-compatibility-and-versioning), [modular](http://typelevel.org/cats/motivations#modularity), [approachable](http://typelevel.org/cats/motivations#approachability) and [efficient](http://typelevel.org//cats/motivations#efficiency). A broader goal of Cats is to provide a foundation for an [ecosystem of pure, typeful libraries](https://typelevel.org/cats/#ecosystem) to support functional programming in Scala applications.

For more detail about Cats' motivations, go [here](http://typelevel.org/cats/motivations).

You can read the API Documentation, [here](https://typelevel.org/cats/api/cats/index.html).

### Getting Started

Cats is currently available for Scala 2.10 (up to 1.2.x), 2.11,  2.12, 2.13.0-M4,  and [Scala.js](http://www.scala-js.org/).


Cats relies on improved type inference via the fix for [SI-2712](https://github.com/scala/bug/issues/2712), which is not enabled by default. For **Scala 2.11.9 or later** you should add the following to your `build.sbt`:

```scala
scalacOptions += "-Ypartial-unification"
```

**Or**, if you need to support older versions of Scala you can use the [sbt-partial-unification](https://github.com/fiadliel/sbt-partial-unification#sbt-partial-unification) plugin which extends support back through **Scala 2.10.6 or later**, to add it, simply add this line to your `plugins.sbt`:

```scala
addSbtPlugin("org.lyranthe.sbt" % "partial-unification" % "1.1.2")
```

And then create the Cats dependency, by adding the following to your `build.sbt`:

```scala
libraryDependencies += "org.typelevel" %% "cats-core" % "1.4.0"
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
 * `alleycats-core`: Cats instances and classes which are not lawful. 
 
 There are several other Cats modules that are in separate repos so that they can 
 maintain independent release cycles. 
 
 * [`cats-effect`](https://github.com/typelevel/cats-effect): standard `IO` type together with `Sync`, `Async` and `Effect` type classes 
 * [`cats-mtl`](https://github.com/typelevel/cats-mtl): transformer typeclasses for Cats' Monads, Applicatives and Functors.
 * [`mouse`](https://github.com/typelevel/mouse): a small companion to Cats that provides convenient syntax (aka extension methods) 
 * [`kittens`](https://github.com/typelevel/kittens): automatic type class instance derivation for Cats and generic utility functions
 * [`cats-tagless`](https://github.com/typelevel/cats-tagless): Utilities for tagless final encoded algebras
 * [`cats-collections`](https://github.com/typelevel/cats-collections): Data structures which facilitate pure functional programming

Release notes for Cats are available in [CHANGES.md](https://github.com/typelevel/cats/blob/master/CHANGES.md).

*Cats is still under active development. While we don't anticipate any
 major redesigns, changes that are neither source nor binary
 compatible are to be expected in upcoming RC1 and 1.0 releases.*


### <a name="ecosystem" href="#ecosystem"></a>The Cats ecosystem

By sharing the same set of type classes, instances and data types provided by Cats, projects can speak the same "Cats language", and integrate with each other with ease.

#### General purpose libraries to support pure functional programming

 * [cats-par](https://github.com/ChristopherDavenport/cats-par): Abstract type member Parallel instances
 * [droste](https://github.com/andyscott/droste): recursion schemes for Cats
 * [Dsl.scala](https://github.com/ThoughtWorksInc/Dsl.scala): The `!`-notation for creating Cats monadic expressions
 * [eff](https://github.com/atnos-org/eff): functional effects and effect handlers (alternative to monad transformers)
 * [Freestyle](https://github.com/frees-io/freestyle): pure functional framework for Free and Tagless Final apps & libs
 * [iota](https://github.com/frees-io/iota): Fast [co]product types with a clean syntax
 * [Monocle](https://github.com/julien-truffaut/Monocle): an optics library for Scala (and Scala.js) strongly inspired by Haskell Lens.
 * [newts](https://github.com/julien-truffaut/newts): Defines newtypes compatible with Cats type classes
 * [origami](https://github.com/atnos-org/origami): monadic folds
 * [refined](https://github.com/fthomas/refined): simple refinement types for Scala
 * [shims](https://github.com/djspiewak/shims): seamless interoperability for cats and scalaz typeclasses and datatypes

#### Libraries with more specific uses

 * [atto](https://github.com/tpolecat/atto): friendly little text parsers
 * [cats-scalacheck](https://github.com/ChristopherDavenport/cats-scalacheck): cats typeclass instances for scalacheck
 * [cats-time](https://github.com/ChristopherDavenport/cats-time): cats typeclass instances for java time
 * [circe](https://github.com/circe/circe): pure functional JSON library
 * [Ciris](https://github.com/vlovgr/ciris): Lightweight, extensible, and validated configuration loading in Scala
 * [cormorant](https://github.com/ChristopherDavenport/cormorant): CSV handling library for FP
 * [decline](https://github.com/bkirwi/decline): A composable command-line parser
 * [doobie](https://github.com/tpolecat/doobie): a pure functional JDBC layer for Scala
 * [extruder](https://github.com/janstenpickle/extruder): Populate case classes from any data source
 * [fastparse-cats](https://github.com/johnynek/fastparse-cats): cats Monad and Alternative instances for [fastparse](https://github.com/lihaoyi/fastparse)
 * [Fetch](https://github.com/47deg/fetch): efficient data access to heterogeneous data sources
 * [finch](https://github.com/finagle/finch): Scala combinator library for building Finagle HTTP services
 * [Frameless](https://github.com/typelevel/frameless): Expressive types for Spark
 * [FS2](https://github.com/functional-streams-for-scala): compositional, streaming I/O library
 * [fuuid](https://github.com/ChristopherDavenport/fuuid): functional uuid's
 * [github4s](https://github.com/47deg/github4s): wrapper around the GitHub API
 * [grafter](https://github.com/zalando/grafter): dependency-injection library using the `Reader` pattern
 * [gsheets4s](https://github.com/benfradet/gsheets4s): wrapper around the Google Sheets API
 * [hammock](https://github.com/pepegar/hammock): Purely functional HTTP client
 * [henkan](https://github.com/kailuowang/henkan): Type safe conversion between case class instances with similar fields
 * [http4s](https://github.com/http4s/http4s): A minimal, idiomatic Scala interface for HTTP
 * [monadic-html](https://github.com/OlivierBlanvillain/monadic-html): Tiny DOM binding library for Scala.js
 * [Monix](https://github.com/monix/monix): high-performance library for composing asynchronous and event-based programs
 * [linebacker](https://github.com/ChristopherDavenport/linebacker): functional thread pool management
 * [log4cats](https://github.com/ChristopherDavenport/log4cats): functional logging
 * [pureconfig](https://github.com/pureconfig/pureconfig): A boilerplate-free library for loading configuration files
 * [rainier](https://github.com/stripe/rainier): Bayesian inference in Scala
 * [scala-forex](https://github.com/snowplow/scala-forex): exchange rate lookups
 * [scala-maxmind-ip-lookups](https://github.com/snowplow/scala-maxmind-iplookups): IP geolocation through [the Maxmind database](https://www.maxmind.com/en/home)
 * [scala-referer-parser](https://github.com/snowplow-referer-parser/scala-referer-parser): referer parsing
 * [scala-weather](https://github.com/snowplow/scala-weather): weather lookups
 * [scanamo](https://github.com/guardian/scanamo): simpler DynamoDB access for Scala
 * [seals](https://github.com/durban/seals): tools for schema evolution and language-integrated schemata
 * [tsec](https://github.com/jmcardon/tsec/): Typesafe, functional, general purpose cryptography and security library
  
Your project talks cats too? [Submit a PR to add it here!](https://github.com/typelevel/cats/edit/master/README.md)

*The full-size [Cats logo](https://typelevel.org/cats/img/cats-logo.png) is available for use for Cats related projects, contents, souvenirs, etc.*

*We offer a [Cats Friendly Badge](https://typelevel.org/cats/img/cats-badge.svg) to let others know your project works with Cats!*

![Cats Friendly Badge](https://typelevel.org/cats/img/cats-badge-normal.png) 

Below are quick html and markdown snippets to use the badge in your own project.
```html
<a href="https://typelevel.org/cats/"><img src="https://typelevel.org/cats/img/cats-badge-tiny.png" alt="Cats friendly" /></a>
```
```markdown
![Cats Friendly Badge](https://typelevel.org/cats/img/cats-badge-tiny.png) 
```

### How can I contribute to Cats?

We welcome contributions to Cats and would love for you to help build
Cats. See our [contributor guide](https://typelevel.org/cats/contributing.html) for more
information about how you can get involved.

### Community

Discussion around Cats is currently happening on Github issue and PR pages
as well as in two Gitter channels: 

[Gitter channel cats](https://gitter.im/typelevel/cats) is for general user 
questions and discussions, and 

[Gitter channel cats-dev](https://gitter.im/typelevel/cats-dev)
is dedicated for Cats development related discussions. For people who wants to 
follow closely and/or to participate in the decisions in Cats development, 
this is the room to join. 

You can get an overview of who is working on what
via [Waffle.io](https://waffle.io/typelevel/cats).

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

### Binary compatibility and versioning

After `1.0.0` release, we [decided](https://github.com/typelevel/cats/issues/1233) 
to use *MAJOR.MINOR.PATCH* [Semantic Versioning 2.0.0](http://semver.org/)
going forward, which is different from the *EPOCH.MAJOR.MINOR* scheme common among 
Java and Scala libraries (including the Scala lang). 

Cats strives to provide a solid and stable foundation for an ecosystem of
FP libraries. Thus, we treat backward binary compatibility maintenance with a high priority. 
In semantic versioning, backward breaking change is **ONLY** allowed between *MAJOR* versions.
We will maintain backward binary compatibility between *PATCH* AND *MINOR* versions.
For example, when we release Cats `1.1.0`, it will be backward binary compatible 
with the previous `1.0.x` versions. I.E. the new JAR will be a drop-in replacement for 
the old one. This is critical when your application has a diamond
dependency on Cats - depending on two or more libraries that all depend on Cats. 
If one library upgrades to the new `1.1.0` Cats before the other one does, your 
application still runs thanks to this backward binary compatibility.  

Also worth noting is that according to semantic versioning, 
*MINOR* version Y (x.Y.z | x > 0) MUST be incremented 
if new, backwards compatible functionality is introduced to the public API. 
It MUST be incremented if any public API functionality is marked as deprecated.

Any binary breaking changes will require a *MAJOR* version bump, which we will be very
cautious about. We will also consider using `organization` and package name for major 
versioning in the future. But that decision is yet to be made. 

### Adopters

Here's a (non-exhaustive) list of companies that use Cats in production. Don't see yours? [You can add it in a PR!](https://github.com/typelevel/cats/edit/master/README.md)

- [Abacus Protocol](https://abacusprotocol.com)
- [Anduin Transactions](https://anduintransact.com)
- [Apple Inc. (FEAR team)](https://news.ycombinator.com/item?id=16969118)
- [AutoScout24](https://www.autoscout24.com) 
- [Avast](https://avast.com)
- [Banno Group inside of Jack Henry & Associates](https://banno.com/)
- [Basefarm](https://basefarm.com/)
- [buildo](https://buildo.io)
- [Codacy](https://www.codacy.com/)
- [Codecentric](https://codecentric.de)
- [Colisweb](https://www.colisweb.com/)
- [DriveTribe](https://drivetribe.com/)
- [Dwolla](https://dwolla.com/)
- [Earnest](https://www.earnest.com)
- [eBay Inc.](https://www.ebay.com)
- [Eloquentix](https://eloquentix.com)
- [eSailors](https://www.esailors.de)
- [Evotor Marketplace](https://market.evotor.ru/)
- [e.near](http://enear.co)
- [E.ON](https://eon.com)
- [formation.ai](https://formation.ai)
- [Free2Move](https://free2move.com)
- [HomeAway](https://www.homeaway.com)
- [iHeartRadio](https://iheart.com)
- [ImmobilienScout24](https://www.immobilienscout24.de/)
- [ITV](https://www.itv.com/)
- [Lookout](https://www.lookout.com)
- [Metacommerce](https://www.metacommerce.ru)
- [Nezasa](https://www.nezasa.com)
- [NCR Edinburgh](https://ncredinburgh.com/)
- [Ocado Technology](https://ocadotechnology.com)
- [REA Group](https://www.realestate.com.au/)
- [Rudder](https://rudder.io)
- [Scalac](https://scalac.io)
- [Scala Center](https://scala.epfl.ch)
- [Snowplow Analytics](https://snowplowanalytics.com/)
- [Spiceworks](https://www.spiceworks.com/)
- [Spotahome](https://spotahome.com)
- [Spotify](https://www.spotify.com)
- [SpringerNature](https://www.springernature.com)
- [Stripe](https://stripe.com)
- [Tecsisa](https://www.tecsisa.com)
- [Teikametrics](http://teikametrics.com)
- [The Guardian](https://www.theguardian.com)
- [Underscore Consulting](https://underscore.io/)
- [Wegtam GmbH](https://www.wegtam.com)
- [WeWork](https://www.wework.com)
- [Wix.com](https://www.wix.com)
- [Zalando](https://zalando.com)
- [47 Degrees](https://www.47deg.com)

### Maintainers

The current maintainers (people who can merge pull requests) are:

 * [ceedubs](https://github.com/ceedubs) Cody Allen
 * [rossabaker](https://github.com/rossabaker) Ross Baker
 * [johnynek](https://github.com/johnynek) P. Oscar Boykin
 * [travisbrown](https://github.com/travisbrown) Travis Brown
 * [adelbertc](https://github.com/adelbertc) Adelbert Chang
 * [LukaJCB](https://github.com/LukaJCB) Luka Jacobowitz
 * [peterneyens](https://github.com/peterneyens) Peter Neyens
 * [tpolecat](https://github.com/tpolecat) Rob Norris
 * [non](https://github.com/non) Erik Osheim
 * [mpilquist](https://github.com/mpilquist) Michael Pilquist
 * [milessabin](https://github.com/milessabin) Miles Sabin
 * [djspiewak](https://github.com/djspiewak) Daniel Spiewak
 * [fthomas](https://github.com/fthomas) Frank Thomas
 * [julien-truffaut](https://github.com/julien-truffaut) Julien Truffaut
 * [kailuowang](https://github.com/kailuowang) Kailuo Wang

We are currently following a practice of requiring at least two
sign-offs to merge PRs (and for large or contentious issues we may
wait for more). For typos or other small fixes to documentation we
relax this to a single sign-off.


### Copyright and License

All code is available to you under the MIT license, available at
http://opensource.org/licenses/mit-license.php and also in the
[COPYING](COPYING) file. The design is informed by many other
projects, in particular [Scalaz](https://github.com/scalaz/scalaz).

Copyright the maintainers, 2015-2018.
