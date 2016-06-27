---
layout: default
title:  "FAQ"
section: "faq"
---

# Frequently Asked Questions

## Questions

 * [What imports do I need?](#what-imports)
 * [Why can't the compiler find implicit instances for Future?](#future-instances)
 * [How can I turn my List of `<something>` into a `<something>` of a list?](#traverse)
 * [What does `@typeclass` mean?](#simulacrum)
 * [What do types like `?` and `λ` mean?](#kind-projector)
 * [What does `macro Ops` do? What is `cats.macros.Ops`?](#machinist)
 * [How can I help?](#contributing)

## What imports do I need?<a id="what-imports" href="#what-imports"></a>

The easiest approach to cats imports is to import everything that's commonly needed:

```tut:silent
import cats._
import cats.data._
import cats.implicits._
```

This should be all that you need, but if you'd like to learn more about the details of imports than you can check out the [import guide](imports.html).

## Why can't the compiler find implicit instances for Future?<a id="future-instances" href="#future-instances"></a>

If you have already followed the [imports advice](#what-imports) but are still getting error messages like `could not find implicit value for parameter e: cats.Monad[scala.concurrent.Future]` or `value |+| is not a member of scala.concurrent.Future[Int]`, then make sure that you have an implicit `scala.concurrent.ExecutionContext` in scope. The easiest way to do this is to `import scala.concurrent.ExecutionContext.Implicits.global`, but note that you may want to use a different execution context for your production application.

## How can I turn my List of `<something>` into a `<something>` of a list?<a id="traverse" href="#traverse"></a>

It's really common to have a `List` of values with types like `Option`, `Xor`, or `Validated` that you would like to turn "inside out" into an `Option` (or `Xor` or `Validated`) of a `List`. The `sequence`, `sequenceU`, `traverse`, and `traverseU` methods are _really_ handy for this. You can read more about them in the [Traverse documentation]({{ site.baseurl }}/tut/traverse.html).

## How can I help?<a id="contributing" href="#contributing"></a>

The cats community welcomes and encourages contributions, even if you are completely new to cats and functional programming. Here are a few ways to help out:

- Find an undocumented method and write a ScalaDoc entry for it. See [Arrow.scala]({{ site.sources }}/core/src/main/scala/cats/arrow/Arrow.scala) for some examples of ScalaDoc entries that use [sbt-doctest](https://github.com/tkawachi/sbt-doctest).
- Look at the [code coverage report](https://codecov.io/github/typelevel/cats?branch=master), find some untested code, and write a test for it. Even simple helper methods and syntax enrichment should be tested.
- Find an [open issue](https://github.com/typelevel/cats/issues?q=is%3Aopen+is%3Aissue+label%3Aready), leave a comment on it to let people know you are working on it, and submit a pull request. If you are new to cats, you may want to look for items with the [low-hanging-fruit](https://github.com/typelevel/cats/issues?q=is%3Aopen+is%3Aissue+label%3A%22low-hanging+fruit%22) label.

See the [contributing guide]({{ site.baseurl }}/contributing.html) for more information.
