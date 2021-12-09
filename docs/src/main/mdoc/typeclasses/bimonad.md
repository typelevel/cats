---
layout: docs
title:  "Bimonad"
section: "typeclasses"
source: "core/src/main/scala/cats/Bimonad.scala"
scaladoc: "#cats.Bimonad"
---
# Bimonad

The `Bimonad` trait directly extends `Monad` and `Comonad` without introducing new methods. `Bimonad` is
different from other `Bi` typeclasses like `Bifunctor`, `Bifoldable` or `Bitraverse` where the prefix describes a
`F[_, _]`. The `Bimonad` is a `F[_]` and the `Bi` prefix has a different meaning here: it's both a `Monad` and a `Comonad`.


If you use `Bimonad` as a convenience type such that:
```scala
def f[T[_] : Monad, Comonad, S](fa: T[S]): S
```
is re-written to:
```scala
def f[T[_] : Bimonad, S](fa: T[S]): S
```
keep in mind `Bimonad` has its own added laws so something that is both monadic
and comonadic may not necessarily be a lawful `Bimonad`.

### NonEmptyList as a Bimonad
NonEmptyList is a lawful `Bimonad` so you can chain computations (like a `Monad`) and `extract` the result at the end (like a `Comonad`).

Here is a possible implementation based on existing monad and comonad:
```scala mdoc
import cats._
import cats.data._
import cats.implicits._

implicit def nelBimonad(implicit monad: Monad[NonEmptyList], comonad: Comonad[NonEmptyList]) =
  new Bimonad[NonEmptyList] {

    //use NonEmptyList specific methods for creation and extraction
    override def pure[A](a: A): NonEmptyList[A] =
      NonEmptyList.one(a)

    override def extract[A](fa: NonEmptyList[A]): A =
      fa.head

    //use the coflatMap from the NonEmptyList comonad
    override def coflatMap[A, B](fa: NonEmptyList[A])(f: NonEmptyList[A] => B): NonEmptyList[B] =
      comonad.coflatMap(fa)(f)

    //use the flatMap and tailRecM from the NonEmptyList monad
    override def flatMap[A, B](fa: NonEmptyList[A])(f: A => NonEmptyList[B]): NonEmptyList[B] =
      monad.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => NonEmptyList[Either[A, B]]): NonEmptyList[B] =
      monad.tailRecM(a)(f)
  }
```

Note the equivalence:
```scala mdoc
nelBimonad.pure(true).extract === NonEmptyList.one(true).head
```

Using generic bimonad syntax we could define a function that appends and extracts a configuration:
```scala mdoc
def make[T[_]: Bimonad](config: T[String]): String = 
  config
    .flatMap(c => Bimonad[T].pure(c + " with option A"))
    .flatMap(c => Bimonad[T].pure(c + " with option B"))
    .flatMap(c => Bimonad[T].pure(c + " with option C"))
    .extract
```

This works with one element non-empty lists:
```scala mdoc
make(NonEmptyList.one("config"))
```

`Function0[_]` and `Eval[_]` are also lawful bimonads so the following calls are also valid:
```scala mdoc
make(() => "config")

make(NonEmptyList.one("config"))
```
