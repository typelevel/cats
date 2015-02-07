---
layout: default
title:  "Typeclasses"
section: "typeclasses"
---
# Functor

A functor is a simple (/typeclass) containing a single method:

    def map[A](fa: F[A])(f: A => B): F[B]

This method takes some F containing As, a Function from A => B, and
returns a F[B]. The name of the method `map` should remind you of the
`map` method that exists on many classes in the scala standard
library. some Examples of map functions:

```scala
scala> Option(1).map(_ + 1)
res0: Option[Int] = Some(2)

scala> List(1,2,3).map(_ + 1)
res1: List[Int] = List(2, 3, 4)

scala> Vector(1,2,3).map(_.toString)
res2: scala.collection.immutable.Vector[String] = Vector(1, 2, 3)
```
