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

Concretely, **it is just a clever construction allowing to build a _very simple_ Monad from any `Functor`**.

This forgetful functor takes a `Monad` and _forgets_ its monadic part (ie `flatMap` function) and applicative part (ie `pure` functions) to finally keep the `Functor` part (ie the `map` function). So, reversing everything, the left-adjoint to this forgetful functor is basically a construction that takes a `Functor` and adds the applicative behavior (ie `pure`) and monadic behavior (ie `flatMap`) to it.

In terms of implementation, to build a `Monad` from a `Functor`, we use the following classic & quite simple inductive definition (for the curious: this generalizes the concept of fixed point functor):

```scala
sealed abstract class Free[F[_], A]
case class Pure[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](a: F[Free[F, A]]) extends Free[F, A]
```

In this representation:

- `Pure` allows to build a `Free` from a pure value and is a reification of the applicative `pure` function.
- `Suspend` allows to build a new `Free` by applying the `Functor F` to previous `Free`.

So typically, a `Free` structure looks like:

```scala
Suspend(F(Suspend(F(Suspend(F(....(Pure(a))))))))
```

It is obvious that `Free` is a recursive structure using `A` in `F[A]` as the recursion carrier with a terminal element `Pure`.

From a computational point of view, `Free` recursive structure can be seen as a sequence of operations:
- `Pure` is a simple operation returning a value `A` and it ends the whole computation.
- `Suspend` is a continuation operation that suspends current computation with the suspension `Functor F` (that can represent a command for example) and hands control to the caller. `A` represents a value bound to this computation.


Please note this `Free` construction has the interesting quality of _encoding_ the recursion on the heap instead of the stack as classic functions. This allows to run those `Free` computations in a stack-safe way.


### For the curious ones

If you look at implementation, you will see another member in `Free` representation:

```scala
sealed abstract case class Gosub[S[_], B]() extends Free[S, B] {
  type C
  val a: () => Free[S, C]
  val f: C => Free[S, B]
}
```

`Gosub` represents a call to a subroutine `a` and when `a` is finished, it continues the computation by calling the function `f` with the result of `a`.

It is actually an optimization of `Free` structure allowing to solve the well-known problem of quadratic complexity implied by very deep recursive Free computations. With `Gosub`, `Free` becomes a right associated structure.


## Using Free Monad

As said before, `Free` is very useful to create embedded DSL.

Let's show with a sample representing a program to interact with a KeyValue Store.

### Study your grammar

We have 3 commands:

- `Put` a value associated to a key
- `Get` a value associated to a key
- `Delete` a value associated to a key


### Create ADT representing your grammar

`ADT` is the abstract data type corresponding to your grammar.

```tut
// 1. create your ADT
sealed trait KVStoreA[+Next]
case class Put[Next](key: String, value: String, next: Next) extends KVStoreA[Next]
case class Get[Next](key: String, onResult: String => Next) extends KVStoreA[Next]
case class Delete[Next](key: String, next: Next) extends KVStoreA[Next]
```

Please note the `next: Next` in each command allowing `KVStoreA[_]` to be a `Functor` and providing a carrier for the `Free` recursion.


### Free your ADT

1. Create a `Free` type based on your ADT.

```tut
import cats.free.Free

// 1. Free type definition
type KVStore[A] = Free[KVStoreA, A]
```

2. Prove your carrier is a `Functor`.

```tut
import cats.Functor

// 2. Functor definition
implicit val functor: Functor[KVStoreA] = new Functor[KVStoreA] {
  def map[A, B](kvs: KVStoreA[A])(f: A => B): KVStoreA[B] = kvs match {
    case Put(key, value, next) => Put(key, value, f(next))
    case Get(key, onResult) => Get(key, onResult andThen f)
    case Delete(key, next) => Delete(key, f(next))
  }
}
```


3. Create smart constructors using `liftF`

```tut
import cats.free.Free.liftF

// 3. smart constructors

// Put is a command returning nothing so its return type is Unit
def put(key: String, value: String): KVStore[Unit] = liftF(Put(key, value, ()))

// Get returns a string
def get(key: String): KVStore[String] = liftF(Get(key, identity))

// Delete is a command returning nothing so its return type is Unit
def delete(key: String): KVStore[Unit] = liftF(Delete(key, ()))

// Update an existing value
def update(key: String, f: String => String): KVStore[Unit] = for {
  v <- get(key)
  _ <- put(key, f(v))
} yield ()
```

4. Build a program

```tut
 // 5. Write program
def program: KVStore[Unit] = for {
  id  <- get("wild-cats")
  _   <- update(id, (_ + 12))
  _   <- delete("kittens")
  _   <- put("alley-cats", "2")
} yield ()
```

5. Write a compiler to your program

TODO

As you may have understood now, `Free` used as an embedded DSL doesn't do anything else than describing a sequence of operations. It just becomes a programming language inside your programming language. So as any programming language, you need to compile it into an _effective_ language that will do the job.

To compile your abstract language into an effective one, you can use a `NaturalTransformation` between your `Functor` and another type container (not necessarily a Functor)


def compile = new (KVStoreA ~> Id) {
  def apply[A](fa: KVStoreA[A]): KVStoreA[A] = {
  }
}

Natural Transformations = interpreters = compiling to another language


6. Run your program stack-safe

TODO

The final step is naturally running your program

Running a Free means folding the structure


## Remarkable kinds of Free

TODO

Trampoline
Option
Iteratee
Source
etc...


## Coyoneda Trick

TODO
