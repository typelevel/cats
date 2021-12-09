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

###Eval as a Bimonad
Eval is a lawful `Bimonad` so you can chain computations (like a `Monad`) and `extract` the result at the end (like a `Comonad`).

Note the equivalence:
```scala mdoc
import cats._
import cats.data._
import cats.implicits._

val evalBimonad = Bimonad[Eval]

evalBimonad.pure(true).extract === Eval.now(true).value
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

This will work with all types of `Eval`:
```scala mdoc
make(Eval.now("config"))

make(Eval.later("config"))
```

`Function0` and `NonEmptyList` are also lawful bimonads so the following calls are also valid:
```scala mdoc
make(() => "config")

make(NonEmptyList.one("config"))
```
