---
layout: default
title:  "Monoid"
section: "typeclasses"
source: "https://github.com/non/algebra/blob/master/core/src/main/scala/algebra/Monoid.scala"

---
# Monoid

`Monoid` extends the [`Semigroup`](semigroup.html) type class, adding an 
`empty` method to semigroup's `combine`. The `empty` method must return a 
value that when combined with any other instance of that type returns the 
other instance, i.e.

```scala
(combine(x, empty) == combine(empty, x) == x)
```
    
For example, if we have a `Monoid[String]` with `combine` defined as string 
concatenation, then `empty = ""`.

Having an `empty` defined allows us to combine all the elements of some 
potentially empty collection of `T` for which a `Monoid[T]` is defined and 
return a `T`, rather than an `Option[T]` as we have a sensible default to 
fall back to.

First some imports.

```tut:silent
import cats._
import cats.std.all._
import cats.implicits._
```

Examples.

```tut
Monoid[String].empty
Monoid[String].combineAll(List("a", "b", "c"))
Monoid[String].combineAll(List())
```

The advantage of using these type class provided methods, rather than the 
specific ones for each type, is that we can compose monoids to allow us to 
operate on more complex types, e.g.
 
```tut
Monoid[Map[String,Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3)))
Monoid[Map[String,Int]].combineAll(List())
```

This is also true if we define our own instances. As an example, let's use 
[`Foldable`](foldable.html)'s `foldMap`, which maps over values accumulating
the results, using the available `Monoid` for the type mapped onto. 

```tut
val l = List(1, 2, 3, 4, 5)
l.foldMap(identity)
l.foldMap(i => i.toString)
```

To use this
with a function that produces a tuple, we can define a `Monoid` for a tuple 
that will be valid for any tuple where the types it contains also have a 
`Monoid` available:

```tut:silent
implicit def tupleMonoid[A : Monoid, B : Monoid]: Monoid[(A, B)] =
   new Monoid[(A, B)] {
     def combine(x: (A, B), y: (A, B)): (A, B) = {
       val (xa, xb) = x
       val (ya, yb) = y
       (Monoid[A].combine(xa, ya), Monoid[B].combine(xb, yb))
     }
     def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)
   }
```

Thus.

```tut
l.foldMap(i => (i, i.toString)) // do both of the above in one pass, hurrah!
```

-------------------------------------------------------------------------------
 
N.B.
Cats does not define a `Monoid` type class itself, it uses the [`Monoid`
trait](https://github.com/non/algebra/blob/master/core/src/main/scala/algebra/Monoid.scala)
which is defined in the [algebra project](https://github.com/non/algebra) on 
which it depends. The [`cats` package object](https://github.com/non/cats/blob/master/core/src/main/scala/cats/package.scala)
defines type aliases to the `Monoid` from algebra, so that you can
`import cats.Monoid`.
