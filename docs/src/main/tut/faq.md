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

The `cats._` import brings in quite a few [type classes](http://typelevel.org/cats/typeclasses.html) (similar to interfaces) such as [Monad](http://typelevel.org/cats/tut/monad.html), [Semigroup](http://typelevel.org/cats/tut/semigroup.html), and [Foldable](http://typelevel.org/cats/tut/foldable.html). Instead of the entire `cats` package, you can import only the types that you need, for example:

```tut:silent
import cats.Monad
import cats.Semigroup
import cats.Foldable
```

The `cats.data._`, import brings in data structures such as [Xor](http://typelevel.org/cats/tut/xor.html), [Validated](http://typelevel.org/cats/tut/validated.html), and [State](http://typelevel.org/cats/tut/state.html). Instead of the entire `cats.data` package, you can import only the types that you need, for example:

```tut:silent
import cats.data.Xor
import cats.data.Validated
import cats.data.State
```

The `cats.implicits._` import does a couple of things. Firstly, it brings in implicit type class instances for standard library types - so after this import you will have `Monad[List]` and `Semigroup[Int]` instances in implicit scope. Secondly, it adds syntax enrichment onto certain types to provide some handy methods, for example:

```tut:book
// cats adds a toXor method to the standard library's Either
val e: Either[String, Int] = Right(3)
e.toXor

// cats adds an orEmpty method to the standard library's Option
val o: Option[String] = None
o.orEmpty
```

**Note**: if you import `cats.implicits._` (the preferred method), you should _not_ also use imports like `cats.syntax.option._` or `cats.instances.either._`. This can result in ambiguous implicit values that cause bewildering compile errors.

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
