---
layout: default
title:  "Applicative"
section: "typeclasses"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/Applicative.scala"
scaladoc: "#cats.Applicative"
---
# Applicative

Applicative functor are a generalization of Monads allowing expressing effectful computations into a pure functional way.
Applicative functors are generally preferred to monads when the structure of a computation is fixed a priori.
That makes it possible to perform certain kinds of static analysis on applicative values