# NonEmptyTraverse

API Documentation: @:api(cats.NonEmptyTraverse)

`NonEmptyTraverse` is a non-empty version of the [Traverse](traverse.md) type class, just like [Reducible](reducible.md) is a non-empty version of [Foldable](foldable.md).
As such, it extends both `Reducible` and `Traverse` in the type class hierarchy.
It provides the `nonEmptyTraverse` and `nonEmptySequence` methods that require an instance of `Apply` instead of `Applicative`:

```scala
def nonEmptyTraverse[G[_]: Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

def nonEmptySequence[G[_]: Apply, A](fga: F[G[A]]): G[F[A]]
```

In the [Applicative tutorial](applicative.md) we learned of `Apply` as a weakened `Applicative` lacking the `pure` method.
One example type lacking an `Applicative` instance is `Map[K, *]`, it's impossible to implement a `pure` method for it.

Knowing this, we can make use of `NonEmptyTraverse`, to traverse over a sequence of `Map`s.
One example application one could think of is, when we have a list of text snippets,
count the occurrence of each word in each snippet and return all the common words and their occurrences in each snippet:

```scala mdoc
import cats.syntax.all._
import cats.data.NonEmptyList

val snippets = NonEmptyList.of("What do you do", "What are you doing")

def countWords(text: String): Map[String, Int] =
  text.split(" ").groupBy(identity).view.mapValues(_.length).toMap

snippets.nonEmptyTraverse(countWords)
```

Note that, just like `traverse`, `nonEmptyTraverse(f)` is equivalent to `map(f).nonEmptySequence`, so the above could be rewritten as:

```scala mdoc
snippets.map(countWords).nonEmptySequence
```

`NonEmptyTraverse` also offers `flatNonEmptyTraverse` and `flatNonEmptySequence` methods that are analogous to `flatTraverse` and `flatSequence` in `Traverse`.
Just like with `nonEmptyTraverse` these methods don't require a `Monad` instance, but only a `FlatMap`, which is the weakened version of `Monad` without `pure`.
