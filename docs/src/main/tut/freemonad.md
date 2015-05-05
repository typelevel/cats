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



## Using Free Monad

If you're interested in the theory behind `Free`, go down, there are a few deeper aspects in terms of Algebra.

If you want to learn by the example, let's start by using `Free` to create embedded DSL (Domain Specific Language).



### Study your topic

So, we want to create a DSL to interact with a KeyValue Store.

The idea is be able to write a static program regrouping a sequence of operations to interact with a KeyValue store,
then compile it and finally execute the program.


For example:

```
put("toto", 3)
get("toto")
delete("toto")
```

But we want:
- the computation to be monadic & pure functionally speaking
- separate the program creation from its compiling from its execution (to be able to choose how we execute it, to debug it etc...)


### Study your grammar

We have 3 commands to interact with our KeyValue store:

- `Put` a value associated to a key
- `Get` a value associated to a key
- `Delete` a value associated to a key


### Create ADT representing your grammar

`ADT` is the Algebraic Data Type corresponding to your program grammar.

```tut
// 1. create your ADT
sealed trait KVStoreA[+Next]
case class Put[T, Next](key: String, value: T, next: Next) extends KVStoreA[Next]
case class Get[T, Next](key: String, onResult: T => Next) extends KVStoreA[Next]
case class Delete[Next](key: String, next: Next) extends KVStoreA[Next]
```

> Please note the `next: Next` in each command providing a carrier for the `Free` recursion (and also allowing `KVStoreA[_]` to be a `Functor`).


### Import Free in your `build.sbt`

```
libraryDependencies +=
  "cats" %% "cats-free" % "0.1.0-SNAPSHOT"
)
```


### Free your ADT

1. Create a `Free` type based on your ADT.

```tut
import cats.free.Free

// 1. Free type definition
type KVStore[A] = Free[KVStoreA, A]
```

2. Prove your carrier is a `Functor`.

One important thing to remark is that `Free[F[_], A]` gives you a **Monad for Free from any Functor F**.

Therefore, we need to prove `KVStoreA` is a functor.

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

so it also means you can use your `Free` structure in a pure monadic way with `for-comprehension`:

```tut
// 5. Write program
def program: KVStore[Int] = for {
  _   <- put("wild-cats", 2)
  _   <- update[Int]("wild-cats", (_ + 12))
  _   <- put("tame-cats", 5)
  id  <- get[Int]("wild-cats")
  _   <- delete("tame-cats")
} yield (id)
```

This looks like a Monadic flow but in fact, it just builds a recursive data structure representing our sequence of operations. Here is a simplification of it:

```
Put("wild-cats", 2, 
  Get("wild-cats",
    Put("wild-cats", f(2),
      Put("tame-cats", 5
        Get("wild-cats",
          Delete("tame-cats",
            Return // to significate the end
          )
        )
      )
    )
  )
)
```

5. Write a compiler for your program

As you may have understood now, `Free` used to create embedded DSL doesn't do anything else than representing your sequence of operations by a recursive data structure.

But, it doesn't produce anything by itself. `Free` is nothing else than a programming language inside your programming language (here a DSL in Scala).

**So as any programming language, you need to compile/interprete it into an _effective_ language and then run it.**

To compile/interprete your abstract language into an effective one, you use a `NaturalTransformation F ~> G` between your `Functor F` to another type container `G`.

Let's write a very simple Natural Transformation that logs & puts our keys/values in a mutable map:

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

Please note this `impureCompiler` is impure as it produces side-effects in a mutable `Map`. As any program, it has side-effects sometimes. The whole purpose of Functional Programming isn't to prevent side-effects, it is just to push side-effects to the boundaries of your system in very well known & controlled parts of your program.

FYI, `Id` represents the simplest type container to extract a final result but you can imagine using any container such as:
- `Future` for asynchronous computation
- `List` for gathering multiple results
- etc...


6. Run your program stack-safe

The final step is naturally running your program after compiling it.

`Free` is just a recursive structure that can be seen as sequence of operations producing other operations. To obtain a result from a sequence, one may think of folding/catamorphing it.


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

**An important aspect of `foldMap` is its stack-safety**: it evaluates each step of computation on the stack then unstack and restart. It will never overflow your stack (except if you do it yourself in your natural transformations). It's heap-intensive but stack-safety allows to use `Free` to represent infinite processes such as streams. 


7. Run program with pure compiler

Previous sample used a effectful natural transformation but you might prefer folding your `Free` in a purer way.

Using an immutable `Map`, it's impossible to write a Natural Transformation using `foldMap` because you need to know the previous state of the `Map` and you don't have it. For this, you need to use the lower level `fold` function and fold the `Free` by yourself.

```tut
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
```

Here you can see a few of scalac limits with respect to pattern matching & JVM type erasure but nothing too hard to go around...


```tut
val result: Map[String, Int] = compilePure(program)
```

## For the curious ones: what is Free in theory?

Mathematically speaking, `FreeMonad` (at least in the programming language context) is a construction that is left adjoint to a forgetful functor whose domain is the category of Monads and whose co-domain is the category of Endofunctors. Hmmmm...

Concretely, **it is just a clever construction allowing to build a _very simple_ Monad from any `Functor`**.

The above forgetful functor takes a `Monad` and:
- forgets its monadic part (ie `flatMap` function)
- forgets its applicative part (ie `pure` functions)
- finally keep the `Functor` part (ie the `map` function).

By reversing all arrows to build the left-adjoint, we deduce that the forgetful functor is basically a construction that:
- takes a `Functor`
- adds the applicative behavior (ie `pure`)
- adds the monadic behavior (ie `flatMap`).

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


### For the very curious ones

If you look at implementation in cats, you will see another member in `Free` representation:

```scala
sealed abstract case class Gosub[S[_], B]() extends Free[S, B] {
  type C
  val a: () => Free[S, C]
  val f: C => Free[S, B]
}
```

`Gosub` represents a call to a subroutine `a` and when `a` is finished, it continues the computation by calling the function `f` with the result of `a`.

It is actually an optimization of `Free` structure allowing to solve the well-known problem of quadratic complexity implied by very deep recursive Free computations. It is exactly the same problem as List appending: the longer the sequence of operations, the longer the `flatMap`. With `Gosub`, `Free` becomes a right associated structure not subject to quadratic complexity.




## Remarkable kinds of Free

TODO

Trampoline
Option
Iteratee
Source
etc...


## Coyoneda Trick

TODO
