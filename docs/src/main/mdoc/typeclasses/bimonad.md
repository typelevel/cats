---
layout: docs
title:  "Bimonad"
section: "typeclasses"
source: "core/src/main/scala/cats/Bimonad.scala"
scaladoc: "#cats.Bimonad"
---
# Bimonad

The `Bimonad` trait directly extends `Monad` and `Comonad` without introducing new behaviour. `Bimonad` is 
different of other `Bi` typeclasses like `Bifunctor`, `Bifoldable` or `Bitraverse` where the prefix describes a
`F[_, _]`. The `Bimonad` is a `F[_]` and could be better seen as a dual monad i.e. something that is both a `Monad` and 
a `Comonad`.

If you use `Bimonad` as a convenience type such that `def f[T[_] : Monad, Comonad, S](fa: T[S]): S` is re-written to
`def f[T[_] : Bimonad, S](fa: T[S]): S` keep in mind `Bimonad` has its own added laws so something that is both monadic 
and comonadic may not necessarily be a lawful `Bimonad`.

###Eval as a Bimonad
Eval is a lawful `Bimonad` so you can chain computations and `extract` the result at the end.

Note the equivalence:
```scala mdoc
import cats._
import cats.data._
import cats.implicits._

Bimonad[Eval].pure(true).extract === Eval.now(true).value
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

//String = config with option A with option B with option C
```

Given that `Function0` or `NonEmptyList` are also lawful bimonads the following calls are also valid:
```scala mdoc
make(() => "config")
make(NonEmptyList.one("config"))

//String = config with option A with option B with option C
```