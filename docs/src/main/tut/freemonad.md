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
 all
Concretely, **it is just a clever construction allowing to build a _very simple_ Monad from any `Functor`**.

For the curious ones, the above forgetful functor takes a `Monad` and _forgets_ its monadic part (ie `flatMap` function) and applicative part (ie `pure` functions) to finally keep the `Functor` part (ie the `map` function). So, by reversing all arrows, the left-adjoint to this forgetful functor is basically a construction that takes a `Functor` and adds the applicative behavior (ie `pure`) and monadic behavior (ie `flatMap`) to it.

In terms of implementation, to build a `Monad` from a `Functor`, we use the following classic & quite simple inductive definition (_this generalizes the concept of fixed point functor_):

```scala
sealed abstract class Free[F[_], A]
case class Pure[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](a: F[Free[F, A]]) extends Free[F, A]
```

In this representation:

- `Pure` allows to build a `Free` from a pure value and is a _reification_ of the applicative `pure` function.
- `Suspend` allows to build a new `Free` by applying the `Functor F` to previous `Free` and permits the monadic `flatMap`.

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

It is actually an optimization of `Free` structure allowing to solve the well-known problem of quadratic complexity implied by very deep recursive Free computations. It is exactly the same problem as List appending: the longer the sequence of operations, the longer the `flatMap`. With `Gosub`, `Free` becomes a right associated structure not subject to quadratic complexity.


## Using Free Monad

As said before, `Free` is very useful to create embedded DSL.

Let's show a sample representing a program to interact with a KeyValue Store.

### Study your grammar

We have 3 commands:

- `Put` a value associated to a key
- `Get` a value associated to a key
- `Delete` a value associated to a key


### Create ADT representing your grammar

`ADT` is the Algebraic Data Type corresponding to your grammar.

```tut
// 1. create your ADT
sealed trait KVStoreA[+Next]
case class Put[T, Next](key: String, value: T, next: Next) extends KVStoreA[Next]
case class Get[T, Next](key: String, onResult: T => Next) extends KVStoreA[Next]
case class Delete[Next](key: String, next: Next) extends KVStoreA[Next]
```

> Please note the `next: Next` in each command allowing `KVStoreA[_]` to be a `Functor` and providing a carrier for the `Free` recursion.


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
    // we need to help a bit scalac here with parametric type
    case g:Get[t, A] => Get[t, B](g.key, g.onResult andThen f)
    case Delete(key, next) => Delete(key, f(next))
  }
}
```


3. Create smart constructors using `liftF`

```tut
import cats.free.Free.liftF
import cats.{Id, ~>}

// 3. smart constructors

// Put is a command returning nothing so its return type is Unit
def put[T](key: String, value: T): KVStore[Unit] = liftF(Put(key, value, ()))

// Get returns a T
def get[T](key: String): KVStore[T] = liftF(Get[T, T](key, identity))

// Delete is a command returning nothing so its return type is Unit
def delete(key: String): KVStore[Unit] = liftF(Delete(key, ()))

// Update an existing value composing two operations
def update[T](key: String, f: T => T): KVStore[Unit] = for {
  v <- get[T](key)
  _ <- put[T](key, f(v))
} yield ()
```

4. Build a program

```tut
// 5. Write program
def program: KVStore[Int] = for {
  _   <- put("wild-cats", 2)
  _   <- update[Int]("wild-cats", (_ + 12))
  id  <- get[Int]("wild-cats")
  _   <- delete("wild-cats")
} yield (id)
```

5. Write a compiler for your program

As you may have understood now, `Free` used as an embedded DSL doesn't do anything else than describing a sequence of operations. But it doesn't produce anything by itself. `Free` is nothing else than a programming language inside your programming language.

**So as any programming language, you need to compile/interprete it into an _effective_ language and then run it.**

To compile/interprete your abstract language into an effective one, you use a `NaturalTransformation F ~> G` between your `Functor F` to another type container `G`.

```tut
// A very dummy state
val m = scala.collection.mutable.Map.empty[String, Any]

// 6. Write compiler
def impureCompiler = new (KVStoreA ~> Id) {

  def apply[A](fa: KVStoreA[A]): Id[A] = fa match {
    case p@Put(key, value, next) => println(s"OP:$p"); m += key -> value; next
    case g:Get[t, A] => println(s"OP:$g"); g.onResult(m(g.key).asInstanceOf[t])
    case d@Delete(key, next) => println(s"OP:$d"); m -= key; next
  }
}

```

This `impureCompiler` is impure as it produces side-effects in a mutable `Map`. But as any program, it has side-effects sometimes. The whole purpose of Functional Programming isn't to prevent side-effects, it is just to push side-effects to the boundaries of your system in very well known & controlled parts of your program.

FYI, `Id` represents the simplest type container that can be extracted (it's a _Comonad_) to retrieve a final result but you can imagine using any container like:
- `Future` for asynchronous computation
- `List` for gathering multiple results


6. Run your program stack-safe

The final step is naturally running your program after compiling it.

`Free` is just a recursive structure that can be seen as sequence of operations producing other operations. To obtain a result from a sequence, one may think of catamorphing/folding it.


The idea behind running a `Free` is exactly the same. We fold the recursive structure by:
- consuming each operation,
- compiling the operation into our effective language using `impureCompiler` potentially applying its effects,
- computing next operation,
- do it recursively until reaching a `Pure` state.

This operation is called `Free.foldMap`:

```scala
  final def foldMap[M[_]](f: S ~> M)(implicit S: Functor[S], M: Monad[M]): M[A] = ...
```

`M` must be a `Monad` to be flattenable (the famous monoid aspect under `Monad`). As `Id` is a `Monad`, we can use `foldMap`.

To run your `Free` with previous `impureCompiler`:

```tut
val result: Id[Int] = program.foldMap(impureCompiler)
```

Let's run it for real:

```
scala> val result: Id[Int] = program.foldMap(impureCompiler)
OP:Put(wild-cats,2,cats.free.Free$$anon$1@73541f26)
OP:Put(alley-cats,23,cats.free.Free$$anon$1@7eb37763)
OP:Get(wild-cats,<function1>)
OP:Put(wild-cats,14,cats.free.Free$$anon$1@719c0f5b)
OP:Get(wild-cats,<function1>)
OP:Delete(alley-cats,cats.free.Free$$anon$1@3a55fa44)
result: cats.Id[Int] = 14
```

**An important aspect of `foldMap` is its stack-safety**: it evaluates each step of computation on the stack then unstack and restart. It will never overflow your stack (except if you do it yourself in your natural transformations). It's heap-intensive but stack-safety allows to use `Free` to represent infinite processes such as streams. 

> By the way, `scalaz-stream` is just actually an optimized version of `Free`.


7. Run program with pure compiler

Previous sample used a effectful natural transformation but you might prefer folding your `Free` in a purer way.

Using an immutable `Map`, it's impossible to write a Natural Transformation using `foldMap` because you need to know the previous state of the `Map` and you don't have it. For this, you need to use the lower level `fold` function and fold the `Free` by yourself.

// Pure computation
def compilePure[A](program: KVStore[A], m: Map[String, A] = Map.empty[String, A]): Map[String, A] = program.fold(
  _ => m,
  {
    // help a bit scalac due to type erasure
    case Put(key, value, next) => compilePure[A](next, m + (key -> value.asInstanceOf[A]))
    // help a bit more scalac here
    case g:Get[a, f] => compilePure(g.onResult(m(g.key).asInstanceOf[a]), m)
    case Delete(key, next) => compilePure(next, m - key)
  }
)

Here you can see a few of scalac limits with respect to pattern matching & JVM type erasure but nothing too hard to go around...


```
scala> val result: Map[String, Int] = compilePure(program)
result: Map[String,Int] = Map(wild-cats -> 14)
```


## Remarkable kinds of Free

TODO

Trampoline
Option
Iteratee
Source
etc...


## Coyoneda Trick

TODO
