## Cats

![Cats graphic](https://typelevel.org/cats/img/cats2.png)

[![cats-core Scala version support](https://index.scala-lang.org/typelevel/cats/cats-core/latest-by-scala-version.svg?targetType=Js)](https://index.scala-lang.org/typelevel/cats/cats-core) ![Continuous Integration](https://github.com/typelevel/cats/workflows/Continuous%20Integration/badge.svg)
[![Financial Contributors on Open Collective](https://opencollective.com/typelevel/all/badge.svg?label=financial+contributors)](https://opencollective.com/typelevel)
[![Discord](https://img.shields.io/discord/632277896739946517.svg?label=&logo=discord&logoColor=ffffff&color=404244&labelColor=6A7EC2)](https://discord.gg/XF3CXcMzqD)

### Overview

Cats is a library which provides abstractions for functional programming in the [Scala programming language](https://scala-lang.org).

Scala supports both object-oriented and functional programming, and this is reflected in the hybrid approach of the
standard library. Cats strives to provide functional programming abstractions that are core, [binary compatible](#binary-compatibility-and-versioning), [modular](https://typelevel.org/cats/motivations.html#modularity), [approachable](https://typelevel.org/cats/motivations.html#approachability) and [efficient](https://typelevel.org/cats/motivations.html#efficiency). A broader goal of Cats is to provide a foundation for an [ecosystem of pure, typeful libraries](https://typelevel.org/cats/typelevelEcosystem.html) to support functional programming in Scala applications.

For more detail about Cats' motivations, go [here](https://typelevel.org/cats/motivations.html).

### Why "cats"?

The name is a playful shortening of the word *category*, from "[category theory](https://en.wikipedia.org/wiki/Category_theory)".

Regardless, you do not need to know anything about category theory to use Cats.

## Contributors

### Code Contributors

This project exists thanks to [all the people who contribute](https://github.com/typelevel/cats/graphs/contributors). We welcome contributions to Cats and would love for you to help build
Cats. See our [contributor guide](CONTRIBUTING.md) for more
information about how you can get involved as a developer. If you are looking for something to start with, [here is a beginner friendly list](https://github.com/typelevel/cats/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

### Financial Contributors

[Become a financial contributor](https://opencollective.com/typelevel) and help us sustain our community. Donations directly support office hours for maintainers, better documentation and strategic initiatives.

<h4>Platinum Sponsors</h4>
<a href="https://opencollective.com/typelevel/contribute/platinum-sposor-12420/checkout">Platinum sponsorship</a> starts at $950 USD/month.
<div id="platinum-sponsors">
  <noscript>Platinum Sponsors appear here at <a href="">https://typelevel.org/cats</a></noscript>
</div>

<h4>Gold Sponsors</h4>
<a href="https://opencollective.com/typelevel/contribute/gold-sponsor-12419/checkout">Gold Sponsorship</a> starts at $420 USD/month.
<div id="gold-sponsors">
  <noscript>Gold Sponsors appear here at <a href="">https://typelevel.org/cats</a></noscript>
</div>

<h4>Silver Sponsors</h4>
<a href="https://opencollective.com/typelevel/contribute/silver-sponsor-11780/checkout">Silver Sponsorship</a> starts at $180 USD/month.
<div id="silver-sponsors">
  <noscript>Silver Sponsors appear here at <a href="">https://typelevel.org/cats</a></noscript>
</div>

<h4>Backers</h4>
Become a <a href="https://opencollective.com/typelevel/contribute/backer-11779/checkout">Backer</a> with a recurring donation of just $5 USD/month.
<div id="backers">
  <noscript>Backers appear here at <a href="">https://typelevel.org/cats</a></noscript>
</div>

<h4>Other contributors</h4>
We thankfully accept <a href="https://opencollective.com/typelevel/donate">one-time and recurring</a> contributions as well.
<div id="other-contributors">
  <noscript>Other contributors appear here at <a href="">https://typelevel.org/cats</a></noscript>
</div>

<script src="/cats/js/sponsors.js"></script>

### Getting Started

Cats is available for [Scala.js](http://www.scala-js.org/) and [Scala Native](https://www.scala-native.org/), as well as the standard JVM runtime.

Cats relies on improved type inference via the fix for [SI-2712](https://github.com/scala/bug/issues/2712), which is not enabled by default. For **Scala 2.11.9+ or 2.12** you should add the following to your `build.sbt`:

```scala
scalacOptions += "-Ypartial-unification"
```

(Partial unification is on by default since Scala 2.13, the compiler no longer accepts `-Ypartial-unification`)

**Or**, if you need to support older versions of Scala you can use the [sbt-partial-unification](https://github.com/fiadliel/sbt-partial-unification#sbt-partial-unification) plugin which extends support back through **Scala 2.10.6 or later**, to add it, simply add this line to your `plugins.sbt`:

```scala
addSbtPlugin("org.lyranthe.sbt" % "partial-unification" % "1.1.2")
```

And then create the Cats dependency, by adding the following to your `build.sbt`:

```scala
libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"
```

This will pull in the cats-core module. If you require some other
functionality, you can pick-and-choose from amongst these modules
(used in place of `"cats-core"`):

 * `cats-kernel`: Small set of basic type classes (*required*).
 * `cats-core`: Most core type classes and functionality (*required*).
 * `cats-laws`: Laws for testing type class instances.
 * `cats-free`: Free structures such as the free monad, and supporting type classes.
 * `cats-testkit`: lib for writing tests for type class instances using laws.
 * `algebra`: Type classes to represent algebraic structures.
 * `alleycats-core`: Cats instances and classes which are not lawful.

 There are several other Cats modules that are in separate repos so that they can
 maintain independent release cycles.

 * [`cats-effect`](https://github.com/typelevel/cats-effect): standard `IO` type together with `Sync`, `Async` and `Effect` type classes
 * [`cats-mtl`](https://github.com/typelevel/cats-mtl): transformer typeclasses for Cats' Monads, Applicatives and Functors.
 * [`mouse`](https://github.com/typelevel/mouse): a small companion to Cats that provides convenient syntax (aka extension methods)
 * [`kittens`](https://github.com/typelevel/kittens): automatic type class instance derivation for Cats and generic utility functions
 * [`cats-tagless`](https://github.com/typelevel/cats-tagless): Utilities for tagless final encoded algebras
 * [`cats-collections`](https://github.com/typelevel/cats-collections): Data structures which facilitate pure functional programming
 * [`cats-testkit-scalatest`](https://github.com/typelevel/cats-testkit-scalatest): Cats testkit integration with Scalatest

Past release notes for Cats are available in [CHANGES.md](https://github.com/typelevel/cats/blob/main/CHANGES.md).

### Documentation

Links:

1. Website: [typelevel.org/cats/](https://typelevel.org/cats/)
2. ScalaDoc: [typelevel.org/cats/api/](https://typelevel.org/cats/api/)
3. Type classes: [typelevel.org/cats/typeclasses](https://typelevel.org/cats/typeclasses.md)
4. Data types: [typelevel.org/cats/datatypes.html](https://typelevel.org/cats/datatypes.md)
5. Algebra overview: [typelevel.org/cats/algebra.html](https://typelevel.org/cats/algebra.md)
6. Glossary: [typelevel.org/cats/nomenclature.html](https://typelevel.org/cats/nomenclature.md)
7. Resources for Learners: [typelevel.org/cats/resources_for_learners.html](https://typelevel.org/cats/resources_for_learners.md)
8. FAQ: [typelevel.org/cats/faq.html](https://typelevel.org/cats/faq.md)
9. The Typelevel Ecosystem: [typelevel.org/cats/typelevelEcosystem.html](https://typelevel.org/cats/typelevelEcosystem.md)

### Community

Discussion around Cats is currently happening on GitHub issues, PR pages,
and Discord:

The [Typelevel Discord](https://discord.gg/XF3CXcMzqD) has \#cats and \#cats-dev
channels, as well as community channels such as \#beginners.  Please join us!

People are expected to follow the
[Scala Code of Conduct](https://www.scala-lang.org/conduct/) when
discussing Cats on GitHub, Discord, or other venues.

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

Here's a (non-exhaustive) list of companies that use Cats in production. Don't see yours? [You can add it in a PR!](https://github.com/typelevel/cats/edit/main/README.md) And if you can, consider [supporting us](https://opencollective.com/typelevel).

- [47 Degrees](https://www.47deg.com)
- [Abacus Protocol](https://abacusprotocol.com)
- [Adobe (CCA team)](https://experienceleague.adobe.com/docs/analytics-platform/using/cja-connections/cca/overview.html?lang=en)
- [Anduin Transactions](https://anduintransact.com)
- [Apple Inc. (FEAR team)](https://news.ycombinator.com/item?id=16969118)
- [AutoScout24](https://www.autoscout24.com)
- [AvantStay](https://avantstay.com/)
- [Avast](https://avast.com)
- [Azavea](https://www.azavea.com/)
- [BabylonHealth](https://www.babylonhealth.com/)
- [Banno Group inside of Jack Henry & Associates](https://banno.com/)
- [Basefarm](https://basefarm.com/)
- [buildo](https://buildo.io)
- [BUX](https://getbux.com/)
- [Chartboost](https://www.chartboost.com/)
- [ClearScore](https://clearscore.com/)
- [Cleverbase](https://cleverbase.com/en/)
- [Codacy](https://www.codacy.com/)
- [Code Dx](https://codedx.com/)
- [Codecentric](https://codecentric.de)
- [Cognite](https://www.cognite.com)
- [Colisweb](https://www.colisweb.com/)
- [CompStak](https://compstak.com)
- [Coya](https://coya.com/)
- [Datum Brain](https://datumbrain.com/)
- [Disney](https://disney.com/)
- [Dripower LTD](https://zd.drip.im)
- [DriveTribe](https://drivetribe.com/)
- [Dwolla](https://dwolla.com/)
- [e.near](http://enear.co)
- [E.ON](https://eon.com)
- [Earnest](https://www.earnest.com)
- [eBay Inc.](https://www.ebay.com)
- [Eloquentix](https://eloquentix.com)
- [eSailors](https://www.esailors.de)
- [Evolution Gaming](https://www.evolutiongaming.com/)
- [Evotor Marketplace](https://market.evotor.ru/)
- [Exelonix](https://exelonix.com/)
- [formation.ai](https://formation.ai)
- [Free2Move](https://free2move.com)
- [Gemini Observatory](https://www.gemini.edu)
- [HelloSoda](https://hellosoda.com)
- [HiFi](https://hi.fi)
- [HomeAway](https://www.homeaway.com)
- [Hootsuite Inc.](https://hootsuite.com/)
- [iHeartRadio](https://iheart.com)
- [ImmobilienScout24](https://www.immobilienscout24.de/)
- [Immozentral](https://www.immozentral.com/)
- [ING Bank](https://www.ing.com)
- [Inner Product](https://inner-product.com)
- [innFactory GmbH](https://www.innfactory.de/)
- [Intent HQ](https://www.intenthq.com/)
- [iofod](https://www.iofod.com/)
- [Iterators](https://www.iteratorshq.com/)
- [ITV](https://www.itv.com/)
- [Kaluza](https://www.kaluza.com)
- [Lego](https://lego.com)
- [Lightform](https://lightform.com/)
- [Lookout](https://www.lookout.com)
- [Medidata](https://www.medidata.com/)
- [Merit](https://merits.com)
- [Metacommerce](https://www.metacommerce.ru)
- [MYOB](https://www.myob.com)
- [NCR Edinburgh](https://ncredinburgh.com/)
- [Netflix](https://jobs.netflix.com)
- [NetworkedAssets](https://www.networkedassets.com/)
- [Nezasa](https://www.nezasa.com)
- [Ocado Technology](https://ocadotechnology.com)
- [OVO Energy](https://ovoenergy.com)
- [Packlink](https://packlink.com/)
- [Permutive](https://permutive.com/)
- [Precog](https://precog.com)
- [Raiffeisenbank Russia](https://www.raiffeisen.ru/)
- [Rakuten](https://www.rakuten.com)
- [RBC Capital Markets](https://www.rbccm.com)
- [REA Group](https://www.realestate.com.au/)
- [Reality Games](https://wearerealitygames.com)
- [Revonte](https://revonte.com)
- [Rewards Network](https://rewardsnetwork.com)
- [RMS](https://www.rms.com/)
- [Rudder](https://rudder.io)
- [Scala Center](https://scala.epfl.ch)
- [Scalac](https://scalac.io)
- [SecurityScorecard](https://securityscorecard.io/)
- [SEEK Limited](https://www.seek.com.au/)
- [Snowplow Analytics](https://snowplowanalytics.com/)
- [SoftwareMill](https://softwaremill.com)
- [Spiceworks](https://www.spiceworks.com/)
- [Spotahome](https://spotahome.com)
- [Spotify](https://www.spotify.com)
- [SpotX](https://www.spotx.tv/)
- [SpringerNature](https://www.springernature.com)
- [SRF](https://www.srf.ch)
- [Standard Chartered](https://www.sc.com)
- [Stripe](https://stripe.com)
- [SWCoE at Philips](https://www.philips.com/)
- [SWOP](https://swop.cx/)
- [Tapad](https://www.tapad.com/)
- [Tecsisa](https://www.tecsisa.com)
- [Teikametrics](http://teikametrics.com)
- [Tenable](https://www.tenable.com)
- [The Guardian](https://www.theguardian.com)
- [Tinkoff](https://tinkoff.ru)
- [Tranzzo](https://tranzzo.com)
- [Underscore Consulting](https://underscore.io/)
- [vidIQ](https://vidiq.com/)
- [Waves Platform](https://wavesplatform.com/)
- [Wegtam GmbH](https://www.wegtam.com)
- [Wejo](https://www.wejo.com/)
- [WeWork](https://www.wework.com)
- [Wix.com](https://www.wix.com)
- [Wolt](https://wolt.com/)
- [XITE](https://xite.com/)
- [Zalando](https://zalando.com)
- [Zendesk](https://www.zendesk.com)

### Maintainers

The current maintainers (people who can merge pull requests) are:

 * [rossabaker](https://github.com/rossabaker) Ross Baker
 * [armanbilge](https://github.com/armanbilge) Arman Bilge
 * [johnynek](https://github.com/johnynek) P. Oscar Boykin
 * [adelbertc](https://github.com/adelbertc) Adelbert Chang
 * [danicheg](https://github.com/danicheg) Daniel Esik
 * [LukaJCB](https://github.com/LukaJCB) Luka Jacobowitz
 * [peterneyens](https://github.com/peterneyens) Peter Neyens
 * [tpolecat](https://github.com/tpolecat) Rob Norris
 * [non](https://github.com/non) Erik Osheim
 * [barambani](https://github.com/barambani) Filippo Mariotti
 * [mpilquist](https://github.com/mpilquist) Michael Pilquist
 * [milessabin](https://github.com/milessabin) Miles Sabin
 * [djspiewak](https://github.com/djspiewak) Daniel Spiewak
 * [fthomas](https://github.com/fthomas) Frank Thomas
 * [satorg](https://github.com/satorg) Sergey Torgashov
 * [julien-truffaut](https://github.com/julien-truffaut) Julien Truffaut
 * [kailuowang](https://github.com/kailuowang) Kailuo Wang

Retired committers include:

 * [ceedubs](https://github.com/ceedubs) Cody Allen
 * [travisbrown](https://github.com/travisbrown) Travis Brown

We are currently following a practice of requiring at least two
sign-offs to merge code PRs (and for large or contentious issues we may
wait for more). For typos, documentation improvements or minor build fix we
relax this to a single sign-off. More detail in the [process document](https://github.com/typelevel/cats/blob/main/PROCESS.md).




### Copyright and License

All code is available to you under the MIT license, available at
http://opensource.org/licenses/mit-license.php and also in the
[COPYING](https://github.com/typelevel/cats/blob/main/COPYING) file. The design is informed by many other
projects, in particular [Scalaz](https://github.com/scalaz/scalaz).

Copyright the maintainers, 2015-2022.
