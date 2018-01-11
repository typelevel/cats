---
layout: docs
title:  "Reducible"
section: "typeclasses"
source: "core/src/main/scala/cats/Reducible.scala"
scaladoc: "#cats.Reducible"
---

# Reducible

`Reducible` extends the `Foldable` type class with additional `reduce` methods.

You may have come by one of the `reduce`, `reduceLeft` or `reduceOption` defined in Scala's standard collections.
`Reducible` offers exactly these methods with the guarantee that the collection won't throw an exception due to a collection being empty, without having to reduce to an `Option`.
This can be utilized effectively to derive `maximum` and `minimum` methods from `Reducible` instead of the `maximumOption` and `minimumOption` found on `Foldable`.

In essence, `reduce` is like a non-empty `fold`, requiring no initial value.
This makes `Reducible` very useful for abstracting over non-empty collections such as `NonEmptyList` or `NonEmptyVector`.

Analogous to the `Foldable` type class, `Reducible[F]` is implemented in terms of two basic methods in addition to those required by `Foldable`:
  - `reduceLeftTo(fa)(f)(g)` eagerly reduces with an additional mapping function
  - `reduceRightTo(fa)(f)(g)` lazily reduces with an additional mapping function


Now, because `Reducible` does not require an empty value, the equivalent of `fold` and `foldMap`, `reduce` and `reduceMap`, do not require an instance of `Monoid`, but of `Semigroup`.

Furthermore, just like with `foldRight`, `reduceRight` uses the `Eval` data type to lazily reduce the collection.

First some standard imports.

```tut:silent
import cats._
import cats.data._
import cats.implicits._
```

And examples.

```tut:book
Reducible[NonEmptyList].reduce(NonEmptyList.of("a", "b", "c"))
Reducible[NonEmptyList].reduceMap(NonEmptyList.of(1, 2, 4))(_.toString)
Reducible[NonEmptyVector].reduceK(NonEmptyVector.of(List(1,2,3), List(2,3,4)))
Reducible[NonEmptyVector].reduceLeft(NonEmptyVector.of(1,2,3,4))((s,i) => s + i)
Reducible[NonEmptyList].reduceRight(NonEmptyList.of(1,2,3,4))((i,s) => Later(s.value + i)).value
Reducible[NonEmptyList].reduceLeftTo(NonEmptyList.of(1,2,3,4))(_.toString)((s,i) => s + i)
Reducible[NonEmptyList].reduceRightTo(NonEmptyList.of(1,2,3,4))(_.toString)((i,s) => Later(s.value + i)).value
Reducible[NonEmptyList].nonEmptyIntercalate(NonEmptyList.of("a", "b", "c"), ", ")

def countChars(s: String) = s.toCharArray.groupBy(identity).mapValues(_.length)

Reducible[NonEmptyList].nonEmptyTraverse_(NonEmptyList.of("Hello", "World"))(countChars)
Reducible[NonEmptyVector].nonEmptyTraverse_(NonEmptyVector.of("Hello", ""))(countChars)
Reducible[NonEmptyList].nonEmptySequence_(NonEmptyList.of(Map(1 -> 'o'), Map(1 -> 'o')))

```

In addition to `reduce` requiring a `Semigroup` instead of a `Monoid`, `nonEmptyTraverse_` and `nonEmptySequence_` require `Apply` instead of `Applicative`.
Also, `Reducible` offers a `reduceLeftM` method, that is just like `foldM`, but requires a `FlatMap` instance of a `Monad` instance.

```scala
def reduceLeftM[G[_]: FlatMap, A, B](fa: F[A])(f: A => G[B])(g: (B, A) => G[B]): G[B]
```
