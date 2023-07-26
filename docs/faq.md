{% laika.title = FAQ %}

# Frequently Asked Questions

@:navigationTree {
  entries = [ { target = "faq.md", depth = 2 } ]
}


## What imports do I need?

The easiest approach to Cats imports is to import everything that's commonly needed:

```scala mdoc:silent
import cats._
import cats.data._
import cats.syntax.all._
```

This should be all that you need, but if you'd like to learn more about the details of imports than you can check out the [import guide](imports.md).

## I am new to pure functional programming, what quick wins can I get from Cats?

Please refer to the [jump start guide](jump_start_guide.md).

## What is the difference between Cats and Scalaz?

Cats and [Scalaz](https://github.com/scalaz/scalaz) have the same goal: to facilitate pure functional programming in Scala applications. However the underlying core strategy is different; Scalaz took the approach of trying to provide a single batteries-included *standard library* for FP that powers the Scala applications. Cats, on the other hand, aims to help build an [ecosystem](typelevelEcosystem.md) of pure FP libraries by providing a solid and stable foundation; these libraries can have their own styles and personalities, competing with each other, while at the same time playing nice. It is through this ecosystem of FP libraries (cats included) that Scala applications can be powered with "FP awesome-ness" and beyond by picking whatever best fit their needs.

Based on this core strategy, Cats takes a [modular](motivations.md#modularity) approach and focuses on providing core, [binary compatible](index.md#binary-compatibility-and-versioning), [approachable](motivations.md#approachability) and [efficient](motivations.md#efficiency) abstractions. It provides a welcoming and supportive environment for the [user community](https://discord.gg/XF3CXcMzqD) governed by the [Scala Code of Conduct](https://www.scala-lang.org/conduct/). It also takes great effort in supplying a comprehensive and beginner-friendly [documentation](motivations.md#documentation).

## Where is right-biased Either?
Up through Cats 0.7.x we had `cats.data.Xor`, which was effectively `scala.util.Either`, but right-biased by default and with
a bunch of useful combinators around it. In Scala 2.12.x `Either`
[became right-biased](https://github.com/scala/scala/pull/5135) so we revisited the use of `Xor` and
[decided](https://github.com/typelevel/cats/issues/1192) that in the interest of interoperability, we would remove `Xor` in the Cats 0.8.0 release and
fill in the gaps in the `scala.util.Either` API via
[syntax enrichment](https://github.com/typelevel/cats/blob/main/core/src/main/scala/cats/syntax/either.scala).

This syntax and the type class instances for `Either` can be imported using `cats.syntax._`, which will also bring in syntactic enrichment and instances for other standard library types, or you can import only the `Either` enrichment with `cats.syntax.either._`.

There are a few minor mismatches between `Xor` and `Either`. For example, in some cases you may need to specify a type parameter for an enrichment method on `Either` (such as `leftMap`) even though it was properly inferred for `Xor`, due to `Either` having covariant type parameters.

Similarly, `cats.data.XorT` has been replaced with `cats.data.EitherT`, although since this is a type defined in Cats, you don't need to import syntax or instances for it (although you may need imports for the underlying monad).

## Why is the compiler having trouble with types with more than one type parameter?

When you encounter a situation where the same code works fine with a type with one type parameter, e.g. `List[A]`, but doesn't work with types with more than one, e.g. `Either[A, B]`, you probably hit [SI-2712](https://issues.scala-lang.org/browse/SI-2712). Without going into the details, it's highly recommended to enable a partial SI-2712 fix in your project. The easiest way to achieve that is through this [sbt plugin](https://github.com/fiadliel/sbt-partial-unification).
Cats used to provide mitigation to this issue semi-transparently, but given the fact that the fix is now mainstream, we decided to drop that mitigation machinery in favor of reducing the complexity. See this [issue](https://github.com/typelevel/cats/issues/1073) for details.

## Why is some example code not compiling for me?

A portion of example code requires either the [Kind-projector](https://github.com/typelevel/kind-projector) compiler plugin or partial unification turned on in scalac. The easiest way to turn partial unification on is through this [sbt plugin](https://github.com/fiadliel/sbt-partial-unification).

## Why can't the compiler find implicit instances for Future?

If you have already followed the [imports advice](#what-imports-do-i-need) but are still getting error messages like `could not find implicit value for parameter e: cats.Monad[scala.concurrent.Future]` or `value |+| is not a member of scala.concurrent.Future[Int]`, then make sure that you have an implicit `scala.concurrent.ExecutionContext` in scope. The easiest way to do this is to `import scala.concurrent.ExecutionContext.Implicits.global`, but note that you may want to use a different execution context for your production application.

## Where are implicit instances for `Seq`?

As of `cats-2.3`, instances for `collection.immutable.Seq` are provided by cats.
Mind that, up to `scala-2.12`, `Seq` was an alias for `collection.Seq` and lawful instances can't be provided for it due to its potential mutability.
In `scala-2.13`, `Seq` was changed to `collection.immutable.Seq` which greatly improves `Seq`'s interoperability with cats.

## How can I turn my List of `<something>` into a `<something>` of a list?

It's really common to have a `List` of values with types like `Option`, `Either`, or `Validated` that you would like to turn "inside out" into an `Option` (or `Either` or `Validated`) of a `List`. The `sequence` and `traverse` methods are _really_ handy for this. You can read more about them in the [Traverse documentation](typeclasses/traverse.md).

## Where is ListT?

There are monad transformers for various types, such as [OptionT](datatypes/optiont.md), so people often wonder why there isn't a `ListT`. For example, in the following example, people might reach for `ListT` to simplify making nested `map` and `exists` calls:

```scala mdoc:reset:silent
val l: Option[List[Int]] = Some(List(1, 2, 3, 4, 5))

def isEven(i: Int): Boolean = i % 2 == 0
```

```scala mdoc
l.map(_.map(_ + 1))
l.exists(_.exists(isEven))
```

A naive implementation of `ListT` suffers from associativity issues; see [this gist](https://gist.github.com/tpolecat/1227e22e3161b5816e014c00650f3b57) for an example. It's possible to create a `ListT` that doesn't have these issues, but it tends to be pretty inefficient. For many use-cases, [Nested](https://github.com/typelevel/cats/blob/main/core/src/main/scala/cats/data/Nested.scala) can be used to achieve the desired results.

Here is how we could achieve the effect of the previous example using `Nested`:

```scala mdoc:silent
import cats.data.Nested
import cats.syntax.all._
```

```scala mdoc
val nl = Nested(l)
nl.map(_ + 1)
nl.exists(isEven)
```

We can even perform more complicated operations, such as a `traverse` of the nested structure:

```scala mdoc:silent
import cats.data.ValidatedNel
type ErrorsOr[A] = ValidatedNel[String, A]
def even(i: Int): ErrorsOr[Int] = if (i % 2 == 0) i.validNel else s"$i is odd".invalidNel
```

```scala mdoc
nl.traverse(even)
```

## Where are `Applicative`s for monad transformers?

An `Applicative` instance for `OptionT[F, *]`/`EitherT[F, E, *]`, built without a corresponding `Monad` instance for `F`, would be unlawful, so it's not included. See [the guidelines](guidelines.md#applicative-instances-for-monad-transformers) for a more detailed explanation.

As an alternative, using `.toNested` on the monad transformer is recommended, although its `ap` will still be inconsistent with the Monad instance's.`.

## Where is IO/Task?

In purely functional programming, a monadic `IO` or `Task` type is often used to handle side effects such as file/network IO. In some languages and frameworks, such a type also serves as the primary abstraction through which parallelism is achieved.  Nearly every real-world purely functional application or service is going to require such a data type, and this gives rise to an obvious question: why doesn't Cats include such a type?

The answer is that Cats *does* include an `IO`, it just isn't included in the core library.  The decision was made to split `IO` away from cats-core and (indeed the whole Cats release cycle!) in order to make it easier to ensure modular versioning and compatibility across the ecosystem.  The [cats-effect](https://github.com/typelevel/cats-effect) project defines a type, `cats.effect.IO`, which is intended to be a very minimal, very performant data type for managing synchronous and asynchronous side-effects, integrated into the Cats ecosystem.

However, we acknowledge that this type may not meet everyone's needs. The cats-effect project characterizes the space of side-effect-capturing data types with a set of typeclasses (deriving from `cats.Monad`), and so all such data types are, broadly-speaking, mutually compatible and interchangeable in many generic contexts. For example, [Monix](https://monix.io) provides support for IO, concurrency, and streaming and integrates with the cats-effect type classes.

It may be worth keeping in mind that `IO` and `Task` are pretty blunt instruments (they are essentially the `Any` of side effect management), and you may want to narrow the scope of your effects throughout most of your application. The [free monad](datatypes/freemonad.md) documentation describes a way to abstractly define controlled effects and interpret them into a type such as `IO` or `Task` as late as possible. As more of your code becomes pure through these controlled effects the less it matters which type you end up choosing to represent your side effects.

## What do types like `?` and `λ` mean?

Cats defines a wealth of type classes and type class instances. For a number of the type class and instance combinations, there is a mismatch between the type parameter requirements of the type class and the type parameter requirements of the data type for which the instance is being defined. For example, the `Either` data type is a type constructor with two type parameters. We would like to be able to define a [Monad](typeclasses/monad.md) for `Either`, but the `Monad` type class operates on type constructors having only one type parameter.

**Enter type lambdas!** Type lambdas provide a mechanism to allow one or more of the type parameters for a particular type constructor to be fixed. In the case of `Either` then, when defining a `Monad` for `Either`, we want to fix one of the type parameters at the point where a `Monad` instance is summoned, so that the type parameters line up. As `Either` is right biased, a type lambda can be used to fix the left type parameter and allow the right type parameter to continue to vary when `Either` is treated as a `Monad`.

**Enter [kind-projector](https://github.com/typelevel/kind-projector)!** kind-projector is a compiler plugin which provides a convenient syntax for dealing with type lambdas. The symbols `?` and `λ` are treated specially by kind-projector, and expanded into the more verbose definitions that would be required were it not to be used. You can read more about kind-projector at the [project page](https://github.com/typelevel/kind-projector).

## What is `tailRecM`?

The `FlatMap` type class has a `tailRecM` method with the following signature:

```scala
def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
```

When you are defining a `FlatMap` instance, its `tailRecM` implementation must have two properties in order for the instance to be considered lawful. The first property is that `tailRecM` must return the same result that you would get if you recursively called `flatMap` until you got a `Right` value (assuming you had unlimited stack space—we'll get to that in a moment). In other words, it must give the same result as this implementation:

```scala mdoc:silent
trait Monad[F[_]] {
  def pure[A](x: A): F[A] = ???
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = ???

  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
    flatMap(f(a)) {
      case Right(b) => pure(b)
      case Left(nextA) => tailRecM(nextA)(f)
    }
}
```

The reason we can't simply use this implementation for all type constructors (and the reason that `tailRecM` is useful at all) is that for many monadic types, recursively `flatMap`-ing in this way will quickly exhaust the stack.

`Option` is one example of a monadic type whose `flatMap` consumes stack in such a way that nesting `flatMap` calls deeply enough (usually around a couple thousand levels) will result in a stack overflow. We can provide a stack-safe `tailRecM` implementation for `Option`, though:

```scala mdoc:silent
import cats.FlatMap
import scala.annotation.tailrec

implicit val optionFlatMap: FlatMap[Option] = new FlatMap[Option] {
  def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

  @tailrec
  def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
    case None => None
    case Some(Left(a1)) => tailRecM(a1)(f)
    case Some(Right(b)) => Some(b)
  }
}
```

Now we don't have to worry about overflowing the stack, no matter how many times we have to call `tailRecM` before we get a `Right`.

This is useful because any operation that you would write using recursive `flatMap`s can be rewritten to use `tailRecM`, and if the `FlatMap` instance for your type constructor is lawful, you don't have to worry about stack safety.

The downside is that how you write a lawful `tailRecM` for your type constructor may not always be obvious. For some type constructors, such as `Future`, recursively `flatMap`-ing is already safe, and the first simple implementation above will be lawful. For types like `Option` and `Try`, you'll need to arrange the recursion in such a way that the `tailRecM` calls are tail calls (which you can confirm with Scala's `tailrec` annotation). Collection types require yet another approach (see for example the [implementation for `List`](https://github.com/typelevel/cats/pull/1041/files#diff-e4d8b82ab5544972195d955591ffe18cR31)).

If you're having trouble figuring out how to implement `tailRecM` lawfully, you can try to find an instance in Cats itself for a type that is semantically similar to yours (all of the `FlatMap` instances provided by Cats have lawful, stack-safe `tailRecM` implementations).

In some cases you may decide that providing a lawful `tailRecM` may be impractical or even impossible (if so we'd like to hear about it). For these cases we provide a way of testing all of the monad laws _except_ for the stack safety of `tailRecM`: just replace `MonadTests[F].monad[A, B, C]` in your tests with `MonadTests[F].stackUnsafeMonad[A, B, C]`.


## What does this symbol mean?

Below is a list of symbols used in Cats.

The `~>`, `⊥`, `⊤`, `:<:` and `:≺:` symbols can be imported with `import cats._`.

All other symbols can be imported with `import cats.syntax.all._`

| Symbol                           | Name                     | Nickname         | Type Class              | Signature                                                           |
| -------------------------------- | -------------------------| ---------------- | ----------------------- | --------------------------------------------------------------------|
| `fa *> fb`                       | product right              |                  | `Apply[F[_]]`           | `productR(fa: F[A])(fb: F[B]): F[B]`                              |
| `fa <* fb`                       | product left               |                  | `Apply[F[_]]`           | `productL(fa: F[A])(fb: F[B]): F[A]`                               |
| `x === y`                        | equals                   |                  | `Eq[A]`                 | `eqv(x: A, y: A): Boolean`                                          |
| `x =!= y`                        | not equals               |                  | `Eq[A]`                 | `neqv(x: A, y: A): Boolean`                                         |
| `fa >>= f`                       | flatMap                  |                  | `FlatMap[F[_]]`         | `flatMap(fa: F[A])(f: A => F[B]): F[B]`                             |
| `fa >> fb`                       | followed by              |                  | `FlatMap[F[_]]`         | `>>(fb: => F[B]): F[B]`                              |
| <code>x &#124;-&#124; y</code>   | remove                   |                  | `Group[A]`              | `remove(x: A, y: A): A`                                             |
| `x > y`                          | greater than             |                  | `PartialOrder[A]`       | `gt(x: A, y: A): Boolean`                                           |
| `x >= y`                         | greater than or equal    |                  | `PartialOrder[A]`       | `gteq(x: A, y: A): Boolean`                                         |
| `x < y`                          | less than                |                  | `PartialOrder[A]`       | `lt(x: A, y: A): Boolean`                                           |
| `x <= y`                         | less than or equal       |                  | `PartialOrder[A]`       | `lteq(x: A, y: A): Boolean`                                         |
| <code>x &#124;+&#124; y</code>   | Semigroup combine        |                  | `Semigroup[A]`          | `combine(x: A, y: A): A`                                            |
| `x <+> y`                        | SemigroupK combine       |                  | `SemigroupK[F[_]]`      | `combineK(x: F[A], y: F[A]): F[A]`                                  |
| `f <<< g`                        | Arrow compose            |                  | `Compose[F[_, _]]`      | `compose(f: F[B, C], g: F[A, B]): F[A, C]`                          |
| `f >>> g`                        | Arrow andThen            |                  | `Compose[F[_, _]]`      | `andThen(f: F[A, B], g: F[B, C]): F[A, C]`                          |
| `f &&& g`                        | Arrow merge              |                  | `Arrow[F[_, _]]`        | `merge[A, B, C](f: F[A, B], g: F[A, C]): F[A, (B, C)]`              |
| `f -< g`                         | Arrow combine and bypass |                  | `Arrow[F[_, _]]`        | `combineAndByPass[A, B, C](f: F[A, B], g: F[B, C]): F[A, (B, C)]`   |
| `F ~> G`                         | natural transformation   |                  | `FunctionK[F[_], G[_]]` | `FunctionK` alias                                                   |
| `F :<: G`                        | injectK                  |                  | `InjectK[F[_], G[_]]`   | `InjectK` alias                                                     |
| `F :≺: G`                        | injectK                  |                  | `InjectK[F[_], G[_]]`   | `InjectK` alias                                                     |
| `fa &> fb`                       | parallel product right     |                  | `Parallel[M[_]]`      | `parProductR[A, B](ma: M[A])(mb: M[B]): M[B]`                     |
| `fa <& fb`                       | parallel product left      |                  | `Parallel[M[_]]`      | `parProductL[A, B](ma: M[A])(mb: M[B]): M[A]`                      |
| `⊥`                              | bottom                   |                  | N/A                     | `Nothing`                                                           |
| `⊤`                              | top                      |                  | N/A                     | `Any`                                                               |
| `fa << fb` (Deprecated)          | product left               |                  | `FlatMap[F[_]]`         | `productL(fa: F[A])(fb: F[B]): F[A]`                               |


## How can I test instances against their type classes' laws?

You can find more information [here](typeclasses/lawtesting.md).

## How can I help?

The Сats community welcomes and encourages contributions, even if you are completely new to Сats and functional programming. Here are a few ways to help out:

- Find an undocumented method and write a ScalaDoc entry for it. See [Arrow.scala](https://github.com/typelevel/cats/blob/main/core/src/main/scala/cats/arrow/Arrow.scala) for some examples of ScalaDoc entries that use [sbt-doctest](https://github.com/tkawachi/sbt-doctest).
- Find an [open issue](https://github.com/typelevel/cats/issues?q=is%3Aopen+is%3Aissue+label%3Aready), leave a comment on it to let people know you are working on it, and submit a pull request. If you are new to Сats, you may want to look for items with the [low-hanging-fruit](https://github.com/typelevel/cats/issues?q=is%3Aopen+is%3Aissue+label%3A%22low-hanging+fruit%22) label.

See the [contributing guide](CONTRIBUTING.md) for more information.

## How to try Cats in a REPL?

The easiest way is probably using [Ammonite-REPL](http://ammonite.io/). Install it following the instructions there. Then in the amm console you can type in
```scala
// interp.configureCompiler(_.settings.YpartialUnification.value = true) // If using scala 2.11 or 2.12
import $ivy.`org.typelevel::cats-core:2.1.1`, cats._, cats.data._, cats.implicits._
```
Or if you want, you can add these lines to `~/.ammonite/predef.sc` so that they are enabled every ammonite session.

## Why aren't monad transformers like `OptionT` and `EitherT` covariant like `Option` and `Either`?

Please see [Variance of Monad Transformers](https://typelevel.org/blog/2018/09/29/monad-transformer-variance.html) on the Typelevel blog.
