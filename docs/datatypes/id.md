# Id

API Documentation: [Id](@API_LINK_BASE@cats/index.html#Id[A]=A)

The identity monad can be seen as the ambient monad that encodes the
effect of having no effect. It is ambient in the sense that plain pure
values are values of `Id`.

It is encoded as:

```scala
type Id[A] = A
```

That is to say that the type `Id[A]` is just a synonym for `A`.  We can
freely treat values of type `A` as values of type `Id[A]`, and
vice-versa.

```scala mdoc
import cats._

val x: Id[Int] = 1
val y: Int = x
```

Using this type declaration, we can treat our Id type constructor as a
[`Monad`](../typeclasses/monad.md) and as a [`Comonad`](../typeclasses/comonad.md). The `pure`
method, which has type `A => Id[A]` just becomes the identity
function.  The `map` method from `Functor` just becomes function
application:

```scala mdoc
import cats.Functor

val one: Int = 1
Functor[Id].map(one)(_ + 1)
```

Compare the signatures of `map` and `flatMap` and `coflatMap`:

```scala
  def map[A, B](fa: Id[A])(f: A => B): Id[B]
  def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B]
  def coflatMap[A, B](a: Id[A])(f: Id[A] => B): Id[B]
```

You'll notice that in the flatMap signature, since `Id[B]` is the same
as `B` for all `B`, we can rewrite the type of the `f` parameter to be
`A => B` instead of `A => Id[B]`, and this makes the signatures of the
two functions the same, and, in fact, they can have the same
implementation, meaning that for `Id`, `flatMap` is also just function
application:

```scala mdoc:nest
import cats.Monad

val one: Int = 1
Monad[Id].map(one)(_ + 1)
Monad[Id].flatMap(one)(_ + 1)
```

And that similarly, `coflatMap` is just function application:

```scala mdoc
import cats.Comonad

Comonad[Id].coflatMap(one)(_ + 1)
```
