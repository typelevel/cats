---
layout: docs
title:  "Phantom"
section: "typeclasses"
source: "core/src/main/scala/cats/functor/Phantom.scala"
scaladoc: "#cats.functor.Phantom"
---
# Phantom

The `Phantom` type class is for functors that define a `pmap`
function with the following type:

```scala
def pmap[A, B](fa: F[A]): F[B]
```

It looks like regular (also called `Covariant`) [`Functor`](functor.html)'s `map` or [`Contravariant`](contravariant.html)'s `contramap`,
but with no `f` transformation provided. Essentially, to implement this `F[A]` must not contain any values of type `A`; 
it must be "phantom" in that type parameter. Equivalently, imagine a type `F[A]` which is a covariant `Functor` and also a `Contravariant` functor:

```scala
def map[A, B](fa: F[A])(f: A => B): F[B]
def contramap[A, B](fa: F[A])(g: B => A): F[B]
def pmap[A, B](fa: F[A]): F[B] =
    contramap[Unit, B](map[A, Unit](fa)(_ => ()))(_ => ())
```

Generally speaking, if a type `F[A]` is both a covariant and a contravariant `Functor`, it cannot contain values of type `A`.
Thus, you can implement `pmap` interms of both `contramap` and `pmap`, and a value of type `F[A]` is also 
a value of type `F[B]` for any types `A` and `B`.

The canonical example of a `Phantom` instance is [`Const`](const.html).
