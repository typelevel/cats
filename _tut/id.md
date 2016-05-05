---
layout: default
title:  "Id"
section: "typeclasses"
source: "core/src/main/scala/cats/package.scala"
scaladoc: "#cats.Id$"
---
# Id

The identity monad can be seen as the ambient monad that encodes the
effect of having no effect. It is ambient in the sense that plain pure
values are values of `Id`.

It is encoded as:

```scala
type Id[A] = A
```

That is to say that the type Id[A] is just a synonym for A.  We can
freely treat values of type `A` as values of type `Id[A]`, and
vice-versa.

```scala
scala> import cats._
import cats._

scala> val x: Id[Int] = 1
x: cats.Id[Int] = 1

scala> val y: Int = x
y: Int = 1
```

Using this type declaration, we can treat our Id type constructor as a
[`Monad`](monad.html) and as a [`Comonad`](comonad.html). The `pure`
method, which has type `A => Id[A]` just becomes the identity
function.  The `map` method from `Functor` just becomes function
application:

```scala
scala> import cats.Functor
import cats.Functor

scala> val one: Int = 1
one: Int = 1

scala> Functor[Id].map(one)(_ + 1)
res0: cats.Id[Int] = 2
```

Compare the signatures of `map` and `flatMap` and `coflatMap`:

```scala
  def map[A, B](fa: Id[A])(f: A => B): Id[B]
  def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B]
  def coflatMap[A, B](a: Id[A])(f: Id[A] => B): Id[B]
```

You'll notice that in the flatMap signature, since `Id[B]` is the same
as `B` for all B, we can rewrite the type of the `f` parameter to be
`A => B` instead of `A => Id[B]`, and this makes the signatures of the
two functions the same, and, in fact, they can have the same
implementation, meaning that for `Id`, `flatMap` is also just function
application:

```scala
scala> import cats.Monad
import cats.Monad

scala> val one: Int = 1
one: Int = 1

scala> Monad[Id].map(one)(_ + 1)
res1: cats.Id[Int] = 2

scala> Monad[Id].flatMap(one)(_ + 1)
res2: cats.Id[Int] = 2
```

And that similarly, coflatMap is just function application:

```scala
scala> import cats.Comonad
import cats.Comonad

scala> Comonad[Id].coflatMap(one)(_ + 1)
res3: cats.Id[Int] = 2
```
