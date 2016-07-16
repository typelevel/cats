---
layout: default
title:  "Monoid"
section: "typeclasses"
source: "kernel/src/main/scala/cats/kernel/Monoid.scala"
scaladoc: "#cats.kernel.Monoid"
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
import cats.implicits._
```

Examples.

```tut:book
Monoid[String].empty
Monoid[String].combineAll(List("a", "b", "c"))
Monoid[String].combineAll(List())
```

The advantage of using these type class provided methods, rather than the 
specific ones for each type, is that we can compose monoids to allow us to 
operate on more complex types, e.g.
 
```tut:book
Monoid[Map[String,Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3)))
Monoid[Map[String,Int]].combineAll(List())
```

This is also true if we define our own instances. As an example, let's use 
[`Foldable`](foldable.html)'s `foldMap`, which maps over values accumulating
the results, using the available `Monoid` for the type mapped onto. 

```tut:book
val l = List(1, 2, 3, 4, 5)
l.foldMap(identity)
l.foldMap(i => i.toString)
```

To use this
with a function that produces a tuple, cats also provides a `Monoid` for a tuple
that will be valid for any tuple where the types it contains also have a 
`Monoid` available, thus.

```tut:book
l.foldMap(i => (i, i.toString)) // do both of the above in one pass, hurrah!
```

-------------------------------------------------------------------------------
 
N.B.
Cats defines  the `Monoid` type class in cats-kernel. The [`cats` package object](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/package.scala)
defines type aliases to the `Monoid` from cats-kernel, so that you can
`import cats.Monoid`. Also the `Monoid` instance for tuple is also [implemented in cats-kernel](https://github.com/typelevel/cats/blob/master/project/KernelBoiler.scala), 
cats merely provides it through [inheritance](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/std/tuple.scala).
