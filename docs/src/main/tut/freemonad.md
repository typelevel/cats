---
layout: default
title:  "FreeMonads"
section: "data"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/free/FreeMonad.scala"
scaladoc: "#cats.free.FreeMonad"
---
# Free Monad

## What is it?

`FreeMonad` is a construction allowing to build a `Monad` from any `Functor` and then, as any `Monad`, it can be 
used to represent a computation and manipulate it in a pure way.

In real life, `FreeMonad` is very useful & practical to:

- represent recursive computations that can be run in a stack-safe way,
- build embedded DSL that can be compiled to other languages using natural transformations.

> In cats, `FreeMonad` is abbreviated to `Free` as in most languages.


## What is it in theory?

Mathematically speaking, `FreeMonad` (at least in the programming language context) is a construction that is left adjoint to a forgetful functor whose domain is the category of Monads and whose co-domain is the category of Endofunctors. Hmmmm...

Concretely, **it is just a clever construction allowing to build a _very simple_ Monad from any Functor**.

This forgetful functor takes a `Monad` and _forgets_ its monadic part (ie `flatMap` function) and applicative part (ie `pure` functions) to finally keep the `Functor` part (ie the `map` function). So, the left-adjoint to this forgetful functor is basically a construction that takes a `Functor` and adds the applicative behavior (ie `pure`) and monadic behavior (ie `flatMap`) to it.

In terms of implementation, to build a `Monad` from a `Functor`, we use the following classic & quite simple inductive definition (for the curious: this generalizes the concept of fixed point functor):

```tut
sealed abstract class Free[F[_], A]
case class Pure[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](a: F[Free[F, A]]) extends Free[F, A]
```

In this representation:

- `Pure` allows to build a `Free` from a pure value and is a reification of the applicative `pure` function.
- `Suspend` allows to build a new `Free` by applying the `Functor F` to previous `Free`.

So typically, a `Free` structure looks like:

```
Suspend(F(Suspend(F(Suspend(F(....(Pure(a))))))))
```

It is obvious that `Free` is a recursive structure with a terminal element `Pure`.

From a computational point of view, this recursive structure can be seen as a sequence of operations:
- `Pure` is a simple operation returning a value and it ends the whole computation.
- `Suspend` is a continuation operation that suspends current computation and the transition to next operation is controlled by the `Functor F` using the result of previous operation.


### For the curious ones

If you look at implementation, you will see another member in `Free` representation:

```tut
sealed abstract case class Gosub[S[_], B]() extends Free[S, B] {
  type C
  val a: () => Free[S, C]
  val f: C => Free[S, B]
}
```

`Gosub` represents a call to a subroutine `a` and when `a` is finished, it calls the function `f` with the result of `a`.

It is actually an optimization of `Free` structure allowing to solve a well-known problem of quadratic complexity when you have  due to left association of previous representation.


## Using Free Monad

As said before, `Free` is very useful to create embedded DSL.

Let's show with a sample representing a program to interact with a KeyValue Store.

### Study your grammar

We have 3 commands:

- `Put` a value associated to a key
- `Get` a value associated to a key
- `Delete` a value associated to a key


### Create ADT

`ADT` is the abstract data type corresponding to your grammar.

```tut
// 1. create your ADT
sealed trait KVStore[+Next]
case class Put[Next](key: String, value: String, next: Next) extends KVStore[Next]
case class Get[Next](key: String, onResult: String => Next) extends KVStore[Next]
case class Delete[Next](key: String, next: Next) extends KVStore[Next]
```

Please note the `next: Next` in each command.

### Free your ADT



```tut
import cats.Functor

object KVStore {
  type Script[A] = Free[KVStore, A]
  
  // 2. Functor definition
  implicit val functor: Functor[KVS] = new Functor[KVStore] {
    def map[A, B](kvs: KVStore[A])(f: A => B): KVStore[B] = kvs match {
      case Put(key, value, next) => Put(key, value, f(next))
      case Get(key, onResult) => Get(key, onResult andThen f)
      case Delete(key, next) => Delete(key, f(next))
    }
  }
```

## Compiling our program





