---
layout: default
title:  "Monoid"
section: "typeclasses"
source: "https://github.com/non/algebra/blob/master/core/src/main/scala/algebra/Monoid.scala"

---
# Monoid

`Monoid` extends the [`Semigroup`](semigroup.html) typeclass, adding an `empty` method to semigroup's 
`combine`. The `empty` method must return a value that when combined with any other instance of that type
returns the other instance, i.e.

    (combine(x, empty) == combine(empty, x) == x)
    
For example, if we have a `Monoid[String]` with `combine` defined as string concatenation, then `empty = ""`.

Having an `empty` defined allows us to combine all the elements of some potentially empty collection
of `T` for which a `Monoid[T]` is defined and return a `T`, rather than an `Option[T]` as we have a
sensible default to fall back to.
 
```tut
import cats._
import cats.std.all._

Monoid[String].empty
Monoid[String].combineAll(List("a", "b", "c"))
Monoid[String].combineAll(List())
```

The advantage of using these typeclass provided methods, rather than the specific ones for each
type, is that we can compose monoids to allow us to operate on more complex types, e.g.
 
```tut
import cats._
import cats.std.all._

Monoid[Map[String,Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3)))
Monoid[Map[String,Int]].combineAll(List())
```
 
N.B.
Cats does not define a `Monoid` typeclass itself, it uses the [`Monoid`
trait](https://github.com/non/algebra/blob/master/core/src/main/scala/algebra/Monoid.scala)
which is defined in the [algebra project](https://github.com/non/algebra) on 
which it depends. The [`cats` package object](https://github.com/non/cats/blob/master/core/src/main/scala/cats/package.scala)
defines type aliases to the `Monoid` from algebra, so that you can
`import cats.Monoid`.
