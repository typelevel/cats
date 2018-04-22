---
layout: docs
title:  "ErrorControl"
section: "typeclasses"
source: "core/src/main/scala/cats/ErrorControl.scala"
scaladoc: "#cats.ErrorControl"
---
# ErrorControl

`ErrorControl` is a type class for principled error handling.
It is designed to be a supplement to `MonadError` with more precise typing and defines a relationship between an
error-handling type `F[A]` and a non-error-handling type `G[A]`.

This means a value of `F[A]` is able to produce either a value of `A` or an error of type `E`.
Unlike `MonadError`'s `handleError` method, the `controlError` function defined in this type class
will yield a value that free of any errors, since they've all been handled.

As an example, because `handleError` takes an `F[A]` and also returns an `F[A]` it's fully legal to chain a bunch of these functions:

```tut
import cats.implicits._

Option(42).handleError(_ => -3).handleError(_ => 67).handleError(_ => 99)
```

In an ideal world, our type system should stop us from doing this more than once.
The equivalent of this function, called `intercept`, on `ErrorControl` does fulfill exactly this:

```scala
trait ErrorControl[F[_], G[_], E] {
  ...

  def intercept[A](fa: F[A])(f: E => A): G[A]

}

Now, you can only call `intercept` on a value of `F[A]` and it will give you a value of `G[A]`, allowing no further chaining.
Here's an example using the [EitherT](datatypes/eithert.html) monad transformer:

```tut:book
import cats.data.EitherT

val fa: EitherT[List, String, Int] = EitherT(List(42.asRight, "Error!".asLeft, 7.asRight, "Another error".asLeft))

val correctList: List[Int] = fa.intercept(err => 0)
```



For more on this topic, check out [this blog post about rethinking MonadError.](https://typelevel.org/blog/2018/04/13/rethinking-monaderror.html)
