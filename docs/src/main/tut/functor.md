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

```tut
Option(1).map(_ + 1)
List(1,2,3).map(_ + 1)
Vector(1,2,3).map(_.toString)
```