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
 * [Where is `ListT`?](#listt)
 * [Where is `IO`/`Task`?](#task)
 * [What does `@typeclass` mean?](#simulacrum)
 * [What do types like `?` and `λ` mean?](#kind-projector)
 * [What does `macro Ops` do? What is `cats.macros.Ops`?](#machinist)
 * [How can I help?](#contributing)

## <a id="what-imports" href="#what-imports"></a>What imports do I need?

The easiest approach to cats imports is to import everything that's commonly needed:

```tut:silent
import cats._
import cats.data._
import cats.implicits._
```

This should be all that you need, but if you'd like to learn more about the details of imports than you can check out the [import guide](imports.html).

## <a id="future-instances" href="#future-instances"></a>Why can't the compiler find implicit instances for Future?

If you have already followed the [imports advice](#what-imports) but are still getting error messages like `could not find implicit value for parameter e: cats.Monad[scala.concurrent.Future]` or `value |+| is not a member of scala.concurrent.Future[Int]`, then make sure that you have an implicit `scala.concurrent.ExecutionContext` in scope. The easiest way to do this is to `import scala.concurrent.ExecutionContext.Implicits.global`, but note that you may want to use a different execution context for your production application.

## <a id="traverse" href="#traverse"></a>How can I turn my List of `<something>` into a `<something>` of a list?

It's really common to have a `List` of values with types like `Option`, `Xor`, or `Validated` that you would like to turn "inside out" into an `Option` (or `Xor` or `Validated`) of a `List`. The `sequence`, `sequenceU`, `traverse`, and `traverseU` methods are _really_ handy for this. You can read more about them in the [Traverse documentation]({{ site.baseurl }}/tut/traverse.html).

## <a id="listt" href="#listt"></a>Where is ListT?

There are monad transformers for various types, such as [OptionT]({{ site.baseurl }}/tut/optiont.html), so people often wonder why there isn't a `ListT`. For example, in the following example, people might reach for `ListT` to simplify making nested `map` and `exists` calls:

```tut:reset:silent
val l: Option[List[Int]] = Some(List(1, 2, 3, 4, 5))

def isEven(i: Int): Boolean = i % 2 == 0
```

```tut:book
l.map(_.map(_ + 1))
l.exists(_.exists(isEven))
```

A naive implementation of `ListT` suffers from associativity issues; see [this gist](https://gist.github.com/tpolecat/1227e22e3161b5816e014c00650f3b57) for an example. It's possible to create a `ListT` that doesn't have these issues, but it tends to be pretty inefficient. For many use-cases, [Nested](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/data/Nested.scala) can be used to achieve the desired results.

Here is how we could achieve the effect of the previous example using `Nested`:

```tut:silent
import cats.data.Nested
import cats.implicits._
```

```tut:book
val nl = Nested(l)
nl.map(_ + 1)
nl.exists(isEven)
```

We can even perform more complicated operations, such as a `traverse` of the nested structure:

```tut:silent
import cats.data.ValidatedNel
type ErrorsOr[A] = ValidatedNel[String, A]
def even(i: Int): ErrorsOr[Int] = if (i % 2 == 0) i.validNel else s"$i is odd".invalidNel

```tut:book
nl.traverse(even)
```

## <a id="task" href="#task"></a>Where is IO/Task?

In purely functional programming, a monadic `IO` or `Task` type is often used to handle side effects such as file/network IO. There [have](https://github.com/typelevel/cats/pull/894) [been](https://github.com/typelevel/cats/pull/907) [several](https://github.com/typelevel/cats/issues/1224) GitHub issues/PRs about this and many more Gitter/IRL conversations, but they all seem to arrive at the conclusion that there isn't a clear canonical `IO` or `Task` that serves everyones' needs. Some of the questions that come up are:

- Should tasks be interruptible?
- Should there be a single `Task` that subsumes `IO` and adds support for asynchrony and concurrency, or should there be separate `IO` and `Task` types?
- How should concurrency be managed/configured?
- Is [scala.js](https://www.scala-js.org/) supported?
- Should you be able to block a thread waiting for the result of a task? This is really convenient for tests, but it isn't really compatible with a JavaScript runtime and therefore is an issue for [scala.js](https://www.scala-js.org/).

For some use-cases, a very simple `IO` is the best answer, as it avoids a lot of the overhead and complexity of other solutions. However, other use-cases require low-level concurrency control with asynchrony, resource management, and stack-safety. Considering all of the competing concerns, Cats has opted to not implement its own `IO`/`Task` types and instead encourage users to use a separate library that best serves their use-case.

Here are a couple libraries with `Task` implementations that you may find useful (in no particular order):
- [Monix](https://monix.io/) - Asynchronous Programming for Scala and [Scala.js](https://www.scala-js.org/).
  - The `monix-eval` module provides a full-featured [Task](https://monix.io/docs/2x/eval/task.html) that is both cancellable and Scala.js-compatible.
- [fs2](https://github.com/functional-streams-for-scala/fs2) - Compositional, streaming I/O library for Scala
  - fs2 provides a [Task](https://github.com/functional-streams-for-scala/fs2/blob/series/0.9/core/src/main/scala/fs2/task.scala) that is a convenient option if you are already using fs2. At the time of this writing, Scala.js support is [in progress](https://github.com/functional-streams-for-scala/fs2/issues/500).

It may be worth keeping in mind that `IO` and `Task` are pretty blunt instruments (they are essentially the `Any` of side effect management), and you may want to narrow the scope of your effects throughout most of your application. The [free monad]({{ site.baseurl }}/tut/freemonad.html) documentation describes a way to abstractly define controlled effects and interpret them into a type such as `IO` or `Task` (or even simply `Try`, `Future`, or [`Id`]({{ site.baseurl }}/tut/id.html)) as late as possible. As more of your code becomes pure through these controlled effects the less it matters which type you end up choosing to represent your side effects.

## <a id="simulacrum" href="#simulacrum"></a>What does `@typeclass` mean?

Cats defines and implements numerous type classes. Unfortunately, encoding these type classes in Scala can incur a large amount of boilerplate. To address this, [Simulacrum](https://github.com/mpilquist/simulacrum) introduces `@typeclass`, a macro annotation which generates a lot of this boilerplate. This elevates type classes to a first class construct and increases the legibility and maintainability of the code. Use of simulacrum also ensures consistency in how the type classes are encoded across a project. Cats uses simulacrum wherever possible to encode type classes, and you can read more about it at the [project page](https://github.com/mpilquist/simulacrum).

Note that the one area where simulacrum is intentionally not used is in the `cats-kernel` module. The `cats-kernel` module is intended to be a shared dependency for a number of projects, and as such, it is important that it is both lightweight and very stable from a binary compatibility perspective. At some point there may be a transition from simulacrum to [typeclassic](https://github.com/typelevel/typeclassic), and the binary compatibility of moving between simulacrum and typeclassic is unclear at this point. Avoiding the dependency on simulacrum in `cats-kernel`, provides insulation against any potential binary compatibility problems in such a transition.

## <a id="kind-projector" href="#kind-projector"></a>What do types like `?` and `λ` mean?

Cats defines a wealth of type classes and type class instances. For a number of the type class and instance combinations, there is a mismatch between the type parameter requirements of the type class and the type parameter requirements of the data type for which the instance is being defined. For example, the [Xor]({{ site.baseurl }}/tut/xor.html) data type is a type constructor with two type parameters. We would like to be able to define a [Monad]({{ site.baseurl }}/tut/monad.html) for `Xor`, but the `Monad` type class operates on type constructors having only one type parameter.

**Enter type lambdas!** Type lambdas provide a mechanism to allow one or more of the type parameters for a particular type constructor to be fixed. In the case of `Xor` then, when defining a `Monad` for `Xor`, we want to fix one of the type parameters at the point where a `Monad` instance is summoned, so that the type parameters line up. As `Xor` is right biased, a type lambda can be used to fix the left type parameter and allow the right type parameter to continue to vary when `Xor` is treated as a `Monad`. The right biased nature of `Xor` is discussed further in the [`Xor` documentation]({{ site.baseurl }}/tut/xor.html).

**Enter [kind-projector](https://github.com/non/kind-projector)!** kind-projector is a compiler plugin which provides a convenient syntax for dealing with type lambdas. The symbols `?` and `λ` are treated specially by kind-projector, and expanded into the more verbose definitions that would be required were it not to be used. You can read more about kind-projector at the [project page](https://github.com/non/kind-projector).

## <a id="machinist" href="#machinist"></a>What does `macro Ops` do? What is `cats.macros.Ops`?

`macro Ops` invokes the [Machinist](https://github.com/typelevel/machinist) Ops macro, and is used in cats in a number of places to enrich types with operations with the minimal possible cost when those operations are called in code. Machinist supports an extension mechanism where users of the macro can provide a mapping between symbolic operator names and method names. The `cats.macros.Ops` class uses this extension mechanism to supply the set of mappings that the cats project is interested in.

More about the history of machinist and how it works can be discovered at the [project page](https://github.com/typelevel/machinist), or [this article on the typelevel blog](http://typelevel.org/blog/2013/10/13/spires-ops-macros.html).

## <a id="contributing" href="#contributing"></a>How can I help?

The cats community welcomes and encourages contributions, even if you are completely new to cats and functional programming. Here are a few ways to help out:

- Find an undocumented method and write a ScalaDoc entry for it. See [Arrow.scala]({{ site.sources }}/core/src/main/scala/cats/arrow/Arrow.scala) for some examples of ScalaDoc entries that use [sbt-doctest](https://github.com/tkawachi/sbt-doctest).
- Look at the [code coverage report](https://codecov.io/github/typelevel/cats?branch=master), find some untested code, and write a test for it. Even simple helper methods and syntax enrichment should be tested.
- Find an [open issue](https://github.com/typelevel/cats/issues?q=is%3Aopen+is%3Aissue+label%3Aready), leave a comment on it to let people know you are working on it, and submit a pull request. If you are new to cats, you may want to look for items with the [low-hanging-fruit](https://github.com/typelevel/cats/issues?q=is%3Aopen+is%3Aissue+label%3A%22low-hanging+fruit%22) label.

See the [contributing guide]({{ site.baseurl }}/contributing.html) for more information.
