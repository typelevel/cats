---
layout: page
title:  "FAQ"
section: "faq"
position: 40
---

# Frequently Asked Questions

## Questions
 * [What imports do I need?](#what-imports)
 * [What is the difference between Cats and Scalaz?](#diff-scalaz) 
 * [Where is right-biased `Either`?](#either)
 * [Why is the compiler having trouble with types with more than one type parameter?](#si-2712)
 * [Why can't the compiler find implicit instances for Future?](#future-instances)
 * [Why is some example code not compiling for me?](#example-compile)
 * [How can I turn my List of `<something>` into a `<something>` of a list?](#traverse)
 * [Where is `ListT`?](#listt)
 * [Where is `IO`/`Task`?](#task)
 * [What does `@typeclass` mean?](#simulacrum)
 * [What do types like `?` and `λ` mean?](#kind-projector)
 * [What does `macro Ops` do? What is `cats.macros.Ops`?](#machinist)
 * [What is `tailRecM`?](#tailrecm)
 * [What does this symbol mean?](#symbol)
 * [How can I test instances against their type classes' laws?](#law-testing)
 * [How can I help?](#contributing)
 * [Is there a sbt plugin that facilitate projects based on the Cats ecosystem libraries?](#sbt-catalysts)

## <a id="what-imports" href="#what-imports"></a>What imports do I need?

The easiest approach to cats imports is to import everything that's commonly needed:

```tut:silent
import cats._
import cats.data._
import cats.implicits._
```

This should be all that you need, but if you'd like to learn more about the details of imports than you can check out the [import guide](typeclasses/imports.html).

## <a id="diff-scalaz" href="#diff-scalaz"></a>What is the difference between Cats and Scalaz? 

Cats and [Scalaz](https://github.com/scalaz/scalaz) have the same goal: to facilitate pure functional programming in Scala applications. However the underlying core strategy is different; Scalaz took the approach of trying to provide a single batteries-included *standard library* for FP that powers the Scala applications. Cats, on the other hand, aims to help build an [ecosystem](/cats/#ecosystem) of pure FP libraries by providing a solid and stable foundation; these libraries can have their own styles and personalities, competing with each other, while at the same time playing nice. It is through this ecosystem of FP libraries (cats included) that Scala applications can be powered with "FP awesome-ness" and beyond by picking whatever best fit their needs.

Based on this core strategy, Cats takes a [modular](/cats/motivations#modularity) approach and focuses on providing core, [binary compatible](/cats/#binary-compatibility-and-versioning), [approachable](/cats/motivations#approachability) and [efficient](/cats/motivations#efficiency) abstractions. It provides a welcoming and supportive environment for the [user community](https://gitter.im/typelevel/cats) governed by the [typelevel code of conduct](https://typelevel.org/conduct). It also takes great effort in supplying a comprehensive and beginner-friendly [documentation](/cats/#documentation).
                       

## <a id="either" href="#either"></a>Where is right-biased Either?
Up through Cats 0.7.x we had `cats.data.Xor`, which was effectively `scala.util.Either`, but right-biased by default and with
a bunch of useful combinators around it. In Scala 2.12.x `Either`
[became right-biased](https://github.com/scala/scala/pull/5135) so we revisited the use of `Xor` and
[decided](https://github.com/typelevel/cats/issues/1192) that in the interest of interoperability, we would remove `Xor` in the Cats 0.8.0 release and
fill in the gaps in the `scala.util.Either` API via
[syntax enrichment](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/syntax/either.scala).

This syntax and the type class instances for `Either` can be imported using `cats.implicits._`, which will also bring in syntactic enrichment and instances for other standard library types, or you can import them individually with `cats.syntax.either._` and `cats.instances.either._`.

There are a few minor mismatches between `Xor` and `Either`. For example, in some cases you may need to specify a type parameter for an enrichment method on `Either` (such as `leftMap`) even though it was properly inferred for `Xor`. See the [`Either` section of this guide](http://typelevel.org/cats/datatypes/either.html#either-in-the-small-either-in-the-large) for more information about these issues.

Similarly, `cats.data.XorT` has been replaced with `cats.data.EitherT`, although since this is a type defined in Cats, you don't need to import syntax or instances for it (although you may need imports for the underlying monad).

## <a id="si-2712" href="#si-2712"></a>Why is the compiler having trouble with types with more than one type parameter?

When you encounter a situation where the same code works fine with a type with one type parameter, e.g. List[A], but doesn't work with types with more than one, e.g. Either[A, B], you probably hit [SI-2712](https://issues.scala-lang.org/browse/SI-2712). Without going into the details, it's highly recommended to enable a partial SI-2712 fix in your project. The easiest way to achieve that is through this [sbt plugin](https://github.com/fiadliel/sbt-partial-unification).
Cats used to provide mitigation to this issue semi-transparently, but given the fact that the fix is now mainstream, we decided to drop that mitigation machinery in favor of reducing the complexity. See this [issue](https://github.com/typelevel/cats/issues/1073) for details.

## <a id="example-compile" href="#example-compile"></a>Why is some example code not compiling for me?

A portion of example code requires either the [Kind-projector](https://github.com/non/kind-projector) compiler plugin or partial unification turned on in scalac. The easiest way to turn partial unification on is through this [sbt plugin](https://github.com/fiadliel/sbt-partial-unification).

## <a id="future-instances" href="#future-instances"></a>Why can't the compiler find implicit instances for Future?

If you have already followed the [imports advice](#what-imports) but are still getting error messages like `could not find implicit value for parameter e: cats.Monad[scala.concurrent.Future]` or `value |+| is not a member of scala.concurrent.Future[Int]`, then make sure that you have an implicit `scala.concurrent.ExecutionContext` in scope. The easiest way to do this is to `import scala.concurrent.ExecutionContext.Implicits.global`, but note that you may want to use a different execution context for your production application.

## <a id="traverse" href="#traverse"></a>How can I turn my List of `<something>` into a `<something>` of a list?

It's really common to have a `List` of values with types like `Option`, `Either`, or `Validated` that you would like to turn "inside out" into an `Option` (or `Either` or `Validated`) of a `List`. The `sequence` and `traverse` methods are _really_ handy for this. You can read more about them in the [Traverse documentation]({{ site.baseurl }}/typeclasses/traverse.html).

## <a id="listt" href="#listt"></a>Where is ListT?

There are monad transformers for various types, such as [OptionT]({{ site.baseurl }}/datatypes/optiont.html), so people often wonder why there isn't a `ListT`. For example, in the following example, people might reach for `ListT` to simplify making nested `map` and `exists` calls:

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
```

```tut:book
nl.traverse(even)
```

## <a id="task" href="#task"></a>Where is IO/Task?

In purely functional programming, a monadic `IO` or `Task` type is often used to handle side effects such as file/network IO. In some languages and frameworks, such a type also serves as the primary abstraction through which parallelism is achieved.  Nearly every real-world purely functional application or service is going to require such a data type, and this gives rise to an obvious question: why doesn't cats include such a type?

The answer is that cats *does* include an `IO`, it just isn't included in the core library.  The decision was made to split `IO` away from cats-core and (indeed the whole cats release cycle!) in order to make it easier to ensure modular versioning and compatibility across the ecosystem.  The [cats-effect](https://github.com/typelevel/cats-effect) project defines a type, `cats.effect.IO`, which is intended to be a very minimal, very performant data type for managing synchronous and asynchronous side-effects, integrated into the cats ecosystem.

However, we acknowledge that this type may not meet everyone's needs.  Notably, `cats.effect.IO` does not provide any mechanism for achieving parallel computation (though such functionality can be built on top of it, which is what libraries like [fs2](https://github.com/functional-streams-for-scala/fs2) achieve).  For these and other needs, we would invite you to consider one of the other `Task`-like data types within the cats ecosystem, such as [Monix's `Task`](https://monix.io).  The cats-effect project characterizes the space of side-effect-capturing data types with a set of typeclasses (deriving from `cats.Monad`), and so all such data types are, broadly-speaking, mutually compatible and interchangeable in many generic contexts.

It may be worth keeping in mind that `IO` and `Task` are pretty blunt instruments (they are essentially the `Any` of side effect management), and you may want to narrow the scope of your effects throughout most of your application. The [free monad]({{ site.baseurl }}/datatypes/freemonad.html) documentation describes a way to abstractly define controlled effects and interpret them into a type such as `IO` or `Task` as late as possible. As more of your code becomes pure through these controlled effects the less it matters which type you end up choosing to represent your side effects.

## <a id="simulacrum" href="#simulacrum"></a>What does `@typeclass` mean?

Cats defines and implements numerous type classes. Unfortunately, encoding these type classes in Scala can incur a large amount of boilerplate. To address this, [Simulacrum](https://github.com/mpilquist/simulacrum) introduces `@typeclass`, a macro annotation which generates a lot of this boilerplate. This elevates type classes to a first class construct and increases the legibility and maintainability of the code. Use of simulacrum also ensures consistency in how the type classes are encoded across a project. Cats uses simulacrum wherever possible to encode type classes, and you can read more about it at the [project page](https://github.com/mpilquist/simulacrum).

Note that the one area where simulacrum is intentionally not used is in the `cats-kernel` module. The `cats-kernel` module is intended to be a shared dependency for a number of projects, and as such, it is important that it is both lightweight and very stable from a binary compatibility perspective. At some point there may be a transition from simulacrum to [typeclassic](https://github.com/typelevel/typeclassic), and the binary compatibility of moving between simulacrum and typeclassic is unclear at this point. Avoiding the dependency on simulacrum in `cats-kernel`, provides insulation against any potential binary compatibility problems in such a transition.

## <a id="kind-projector" href="#kind-projector"></a>What do types like `?` and `λ` mean?

Cats defines a wealth of type classes and type class instances. For a number of the type class and instance combinations, there is a mismatch between the type parameter requirements of the type class and the type parameter requirements of the data type for which the instance is being defined. For example, the [Either]({{ site.baseurl }}/datatypes/either.html) data type is a type constructor with two type parameters. We would like to be able to define a [Monad]({{ site.baseurl }}/typeclasses/monad.html) for `Either`, but the `Monad` type class operates on type constructors having only one type parameter.

**Enter type lambdas!** Type lambdas provide a mechanism to allow one or more of the type parameters for a particular type constructor to be fixed. In the case of `Either` then, when defining a `Monad` for `Either`, we want to fix one of the type parameters at the point where a `Monad` instance is summoned, so that the type parameters line up. As `Either` is right biased, a type lambda can be used to fix the left type parameter and allow the right type parameter to continue to vary when `Either` is treated as a `Monad`. The right biased nature of `Either` is discussed further in the [`Either` documentation]({{ site.baseurl }}/datatypes/either.html).

**Enter [kind-projector](https://github.com/non/kind-projector)!** kind-projector is a compiler plugin which provides a convenient syntax for dealing with type lambdas. The symbols `?` and `λ` are treated specially by kind-projector, and expanded into the more verbose definitions that would be required were it not to be used. You can read more about kind-projector at the [project page](https://github.com/non/kind-projector).

## <a id="machinist" href="#machinist"></a>What does `macro Ops` do? What is `cats.macros.Ops`?

`macro Ops` invokes the [Machinist](https://github.com/typelevel/machinist) Ops macro, and is used in cats in a number of places to enrich types with operations with the minimal possible cost when those operations are called in code. Machinist supports an extension mechanism where users of the macro can provide a mapping between symbolic operator names and method names. The `cats.macros.Ops` class uses this extension mechanism to supply the set of mappings that the cats project is interested in.

More about the history of machinist and how it works can be discovered at the [project page](https://github.com/typelevel/machinist), or [this article on the typelevel blog](http://typelevel.org/blog/2013/10/13/spires-ops-macros.html).

## <a id="tailrecm" href="#tailrecm"></a>What is `tailRecM`?

The `FlatMap` type class has a `tailRecM` method with the following signature:

```scala
def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
```

When you are defining a `FlatMap` instance, its `tailRecM` implementation must have two properties in order for the instance to be considered lawful. The first property is that `tailRecM` must return the same result that you would get if you recursively called `flatMap` until you got a `Right` value (assuming you had unlimited stack space—we'll get to that in a moment). In other words, it must give the same result as this implementation:

```tut:silent
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

```tut:silent
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


## <a id="symbol" href="#symbol"></a>What does this symbol mean?

Below is a list of symbols used in cats.

The `~>`, `⊥`, `⊤`, `:<:` and `:≺:` symbols can be imported with `import cats._`.

All other symbols can be imported with `import cats.implicits._`

| Symbol                           | Name                     | Nickname         | Type Class              | Signature                                                                    |
| -------------------------------- | -------------------------| ---------------- | ----------------------- | -----------------------------------------------------------------------------|
| `fa *> fb`                       | followed by              |                  | `Apply[F[_]]`           | `followedBy(fa: F[A])(fb: F[B]): F[B]`                                       |
| `fa <* fb`                       | for effect               |                  | `Apply[F[_]]`           | `forEffect(fa: F[A])(fb: F[B]): F[A]`                                        |
| `x === y`                        | equals                   |                  | `Eq[A]`                 | `eqv(x: A, y: A): Boolean`                                                   |
| `x =!= y`                        | not equals               |                  | `Eq[A]`                 | `neqv(x: A, y: A): Boolean`                                                  |
| `fa >>= f`                       | flatMap                  |                  | `FlatMap[F[_]]`         | `flatMap(fa: F[A])(f: A => F[B]): F[B]`                                      |
| `fa >> fb`                       | followed by              |                  | `FlatMap[F[_]]`         | `>>(fb: => F[B]): F[B]`                                       |
| <code>x &#124;-&#124; y</code>   | remove                   |                  | `Group[A]`              | `remove(x: A, y: A): A`                                                      |
| `x > y`                          | greater than             |                  | `PartialOrder[A]`       | `gt(x: A, y: A): Boolean`                                                    |
| `x >= y`                         | greater than or equal    |                  | `PartialOrder[A]`       | `gteq(x: A, y: A): Boolean`                                                  |
| `x < y`                          | less than                |                  | `PartialOrder[A]`       | `lt(x: A, y: A): Boolean`                                                    |
| `x <= y`                         | less than or equal       |                  | `PartialOrder[A]`       | `lteq(x: A, y: A): Boolean`                                                  |
| <code>x &#124;+&#124; y</code>   | Semigroup combine        |                  | `Semigroup[A]`          | `combine(x: A, y: A): A`                                                     |
| `x <+> y`                        | SemigroupK combine       |                  | `SemigroupK[F[_]]`      | `combineK(x: F[A], y: F[A]): F[A]`                                           |
| `f <<< g`                        | Arrow compose            |                  | `Compose[F[_, _]]`      | `compose(f: F[B, C], g: F[A, B]): F[A, C]`                                   |
| `f >>> g`                        | Arrow andThen            |                  | `Compose[F[_, _]]`      | `andThen(f: F[B, C], g: F[A, B]): F[A, C]`                                   |
| `f &&& g`                        | Arrow merge              |                  | `Arrow[F[_, _]]`        | `merge[A, B, C](f: F[A, B], g: F[A, C]): F[A, (B, C)]`                       |
| `f -< g`                         | Arrow combine and bypass |                  | `Arrow[F[_, _]]`        | `combineAndByPass[A, B, C](f: F[A, B], g: F[B, C]): F[A, (B, C)]`            | 
| `f +++ g`                        | ArrowChoice choose       |                  | `ArrowChoice[F[_, _]]`  | `choose[A, B, C, D](f: F[A, C])(g: F[B, D]): F[Either [A, B], Either[C, D]]` |
| `f ||| g`                        | Choice choice            |                  | `Choice[F[_, _]]`       | `choice[A, B, C](f: F[A, C], g: F[B, C]): F[Either[A, B], C]`            |
| `F ~> G`                         | natural transformation   |                  | `FunctionK[F[_], G[_]]` | `FunctionK` alias                                                            |
| `F :<: G`                        | injectK                  |                  | `InjectK[F[_], G[_]]`   | `InjectK` alias                                                              |
| `F :≺: G`                        | injectK                  |                  | `InjectK[F[_], G[_]]`   | `InjectK` alias                                                              |
| `fa &> fb`                       | parallel followed by     |                  | `Parallel[M[_], F[_]]`  | `parFollowedBy[A, B](ma: M[A])(mb: M[B]): M[B]`                              |
| `fa <& fb`                       | parallel for effect      |                  | `Parallel[M[_], F[_]]`  | `parForEffect[A, B](ma: M[A])(mb: M[B]): M[A]`                               |
| `⊥`                              | bottom                   |                  | N/A                     | `Nothing`                                                                    |
| `⊤`                              | top                      |                  | N/A                     | `Any`                                                                        |
| `fa << fb` (Deprecated)          | for effect               |                  | `FlatMap[F[_]]`         | `forEffect(fa: F[A])(fb: F[B]): F[A]`                                        |

## <a id="law-testing" href="#law-testing"></a>How can I test instances against their type classes' laws?

You can find more information [here](typeclasses/lawtesting.html).

## <a id="contributing" href="#contributing"></a>How can I help?

The cats community welcomes and encourages contributions, even if you are completely new to cats and functional programming. Here are a few ways to help out:

- Find an undocumented method and write a ScalaDoc entry for it. See [Arrow.scala]({{ site.sources }}/core/src/main/scala/cats/arrow/Arrow.scala) for some examples of ScalaDoc entries that use [sbt-doctest](https://github.com/tkawachi/sbt-doctest).
- Look at the [code coverage report](https://codecov.io/github/typelevel/cats?branch=master), find some untested code, and write a test for it. Even simple helper methods and syntax enrichment should be tested.
- Find an [open issue](https://github.com/typelevel/cats/issues?q=is%3Aopen+is%3Aissue+label%3Aready), leave a comment on it to let people know you are working on it, and submit a pull request. If you are new to cats, you may want to look for items with the [low-hanging-fruit](https://github.com/typelevel/cats/issues?q=is%3Aopen+is%3Aissue+label%3A%22low-hanging+fruit%22) label.

See the [contributing guide]({{ site.baseurl }}/contributing.html) for more information.

## <a id="sbt-catalysts" href="#sbt-catalysts"></a>Is there a sbt plugin that facilitate projects based on the Cats ecosystem libraries?

Of course. [sbt-catalysts](https://github.com/typelevel/sbt-catalysts) is created particularly for this purpose. It also provides a g8 template so that you can run `sbt new typelevel/sbt-catalysts.g8` to quickly set up a project using Cats ecosystem libraries through this plugin. For more details, go to [sbt-catalysts](https://github.com/typelevel/sbt-catalysts). 

