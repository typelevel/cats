---
layout: default
title:  "FreeMonads"
section: "data"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/free/FreeMonad.scala"
scaladoc: "#cats.free.FreeMonad"
---

# Free Monad

## What is it?

A *free monad* is a construction which allows you to build a *monad*
from any *Functor*. Like other *monads*, it is a pure way to represent
and manipulate computations.

In particular, *free monads* provide a practical way to:

 - represent stateful computations as data, and run them
 - run recursive computations in a stack-safe way
 - build an embedded DSL (domain-specific language)
 - retarget a computation to another interpreter using natural transformations

> (In cats, the type representing a *free monad* is abbreviated as `Free[_]`.)

## Using Free Monads

A good way to get a sense for how *free monads* work is to see them in
action. The next section uses `Free[_]` to create an embedded DSL
(Domain Specific Language).

If you're interested in the theory behind *free monads*, the
[What is Free in theory?]() section discusses free moands in terms of
category theory.

### Study your topic

Let's imagine that we want to create a DSL for a key-value store. We
want to be able to do three things with keys:

 - *put* a `value` into the store associate with its `key`.
 - *get* a `value` from the store given its `key`.
 - *delete* a value from the store given its `key`.

The idea is to write a sequence of these operations in the embedded
DSL as a "program", compile the "program", and finally execute the
"program" to interact with the actual key-value store.

For example:

```scala
put("toto", 3)
get("toto") // returns 3
delete("toto")
```

But we want:

 - the computation to be represented as a pure, immutable value
 - to separate the creation and execution of the program
 - to be able to support many different methods of execution

### Study your grammar

We have 3 commands to interact with our KeyValue store:

- `Put` a value associated with a key into the store
- `Get` a value associated with a key out of the store
- `Delete` a value associated with a key from the store

### Create an ADT representing your grammar

ADT stands for *Algebraic Data Type*. In this context, it refers to a
closed set of types which can be combined to build up complex,
recursive values.

We need to create an ADT to represent our key-value operations:

```tut
sealed trait KVStoreA[+Next]
case class Put[T, Next](key: String, value: T, next: Next) extends KVStoreA[Next]
case class Get[T, Next](key: String, onResult: T => Next) extends KVStoreA[Next]
case class Delete[Next](key: String, next: Next) extends KVStoreA[Next]
```

The `next` field in each of the types provides a way to link an
operation with successive values. The `Next` type parameter can be
anything at all, including `Unit`. It can be thought of as a carrier,
a way to link a single operation with successive operations.

As we will see, the `next` field is also necessary to allowing us to
provide a `Functor` instance for `KVStoreA[_]`.

### Import Free in your `build.sbt`

```scala
libraryDependencies += "cats" %% "cats-free" % "0.1.0-SNAPSHOT"
```

### Free your ADT

There are six basic steps to "freeing" the ADT:

1. Create a type based on `Free[_]` and `KVStoreA[_]`.
2. Prove `KVStoreA[_]` has a functor.
3. Create smart constructors for `KVStore[_]` using `liftF`.
4. Build a program out of key-value DSL operations.
5. Build a compiler for programs of DSL operations.
6. Execute our compiled program.

#### 1. Create a `Free` type based on your ADT

```tut
import cats.free.Free

type KVStore[A] = Free[KVStoreA, A]
```

#### 2. Prove `KVStoreA[_]` has a `Functor`.

One important thing to remark is that `Free[F[_], A]` automatically
gives us a monad for `F`, if `F` has a functor (i.e. if we can find or
construct a `Functor[F]` instance). It is described as a *free monad*
since we get this monad "for free."

Therefore, we need to prove `KVStoreA[_]` has a functor.

```tut
import cats.Functor

implicit val functor: Functor[KVStoreA] =
  new Functor[KVStoreA] {
    def map[A, B](kvs: KVStoreA[A])(f: A => B): KVStoreA[B] =
      kvs match {
        case Put(key, value, next) =>
          Put(key, value, f(next))
        case g: Get[t, A] => // help scalac with parametric type
          Get[t, B](g.key, g.onResult andThen f)
        case Delete(key, next) =>
          Delete(key, f(next))
      }
  }
```

#### 3. Create smart constructors using `liftF`

These methods will make working with our DSL a lot nicer, and will
lift `KVStoreA[_]` values into our `KVStore[_]` monad (note the
missing "A" in the second type).

```tut
import cats.free.Free.liftF

// Put returns nothing (i.e. Unit).
def put[T](key: String, value: T): KVStore[Unit] =
  liftF(Put(key, value, ()))

// Get returns a T value.
def get[T](key: String): KVStore[T] =
  liftF(Get[T, T](key, identity))

// Delete returns nothing (i.e. Unit).
def delete(key: String): KVStore[Unit] =
  liftF(Delete(key, ()))

// Update composes get and set, and returns nothing.
def update[T](key: String, f: T => T): KVStore[Unit] =
  for {
    v <- get[T](key)
    _ <- put[T](key, f(v))
  } yield ()
```

#### 4. Build a program

Now that we can construct `KVStore[_]` values we can use our DSL to
write "programs" using a *for-comprehension*:

```tut
def program: KVStore[Int] =
  for {
    _ <- put("wild-cats", 2)
    _ <- update[Int]("wild-cats", (_ + 12))
    _ <- put("tame-cats", 5)
    n <- get[Int]("wild-cats")
    _ <- delete("tame-cats")
  } yield n
```

This looks like a Monadic flow. However, it just builds a recursive
data structure representing the sequence of operations. Here is a
similar program represented explicitly:

```tut
val programA =
  Put("wild-cats", 2,
    Get("wild-cats", { (n0: Int) =>
      Put("wild-cats", n0 + 12,
        Put("tame-cats", 5,
          Get("wild-cats", { (n1: Int) =>
            Delete("tame-cats", n1)
          })
        )
      )
    })
  )
```

One problem with `programA` you may have noticed is that the
constructor and function calls are all nested. If we had to sequence
ten thousand operations, we might run out of stack space and trigger a
`StackOverflowException`.

#### 5. Write a compiler for your program

As you may have understood now, `Free[_]` used to create an embedded
DSL. By itself, this DSL only represents a sequence of operations
(defined by a recursive data structure); it doesn't produce anything.

`Free[_]` is a programming language inside your programming language!

**So, like any other programming language, we need to compile our
  abstract language into an _effective_ language and then run it.**

To do this, we will use a *natural transformation* between type
containers.  Natural transformations go between types like `F[_]` and
`G[_]` (this particular transformation would be written as `F ~> G`).

In our case, we will use a simple mutable map to represent our key
value store:

```tut
import cats.{Id, ~>}
import scala.collection.mutable

// a very simple (and imprecise) key-value store
val kvs = mutable.Map.empty[String, Any]

// the program will crash if a key is not found,
// or if a type is incorrectly specified.
def impureCompiler =
  new (KVStoreA ~> Id) {
    def apply[A](fa: KVStoreA[A]): Id[A] =
      fa match {
        case Put(key, value, next) =>
          println(s"put($key, $value)")
          kvs(key) = value
          next
        case g: Get[t, A] =>
          println(s"get(${g.key})")
          g.onResult(kvs(g.key).asInstanceOf[t])
        case Delete(key, next) =>
          println(s"delete($key)")
          kvs.remove(key)
          next
      }
  }
```

Please note this `impureCompiler` is impure -- it mutates `kvs` and
also produces logging output using `println`.  The whole purpose of
functional programming isn't to prevent side-effects, it is just to
push side-effects to the boundaries of your system in a well-known and
controlled way.

`Id[_]` represents the simplest type container: the type itself. Thus,
`Id[Int]` is just `Int`. This means that our program will execute
immediately, and block until the final value can be returned.

However, we could easily use other type containers for different
behavior, such as:

 - `Future[_]` for asynchronous computation
 - `List[_]` for gathering multiple results
 - `Option[_]` to support optional results
 - `Validated[_]` (or `Xor[E, ?]`) to support failure
 - a pseudo-random monad to support non-determinism
 - and so on...

#### 6. Run your program

The final step is naturally running your program after compiling it.

`Free[_]` is just a recursive structure that can be seen as sequence
of operations producing other operations. In this way it is similar to
`List[_]`. We often use folds (e.g. `foldRight`) to obtain a single
value from a list; this recurses over the structure, combining its
contents.

The idea behind running a `Free[_]` is exactly the same. We fold the
recursive structure by:

 - consuming each operation.
 - compiling the operation into our effective language using
  `impureCompiler` (applying its effects if any).
 - computing next operation.
 - continue recursively until reaching a `Pure` state, and returning it.

This operation is called `Free.foldMap`:

```scala
final def foldMap[M[_]](f: S ~> M)(implicit S: Functor[S], M: Monad[M]): M[A] = ...
```

`M` must be a `Monad` to be flattenable (the famous monoid aspect
under `Monad`). As `Id` is a `Monad`, we can use `foldMap`.

To run your `Free` with previous `impureCompiler`:

```tut
val result: Id[Int] = program.foldMap(impureCompiler)
```

An important aspect of `foldMap` is its **stack-safety**. It evaluates
each step of computation on the stack then unstack and restart. This
process is known as trampolining.

As long as your natural transformation is stack-safe, `foldMap` will
never overflow your stack.  Trampolining is heap-intensive but
stack-safety provides the reliability required to use `Free[_]` for
data-intensive tasks, as well as infinite processes such as streams.

#### 7. Use a pure compiler (optional)

The previous examples used a effectful natural transformation. This
works, but you might prefer folding your `Free` in a "purer" way.

Using an immutable `Map`, it's impossible to write a Natural
Transformation using `foldMap` because you would need to know the
previous state of the `Map` and you don't have it. For this, you need
to use the lower level `fold` function and fold the `Free[_]` by
yourself:

```tut
// Pure computation
def compilePure[A](program: KVStore[A], kvs: Map[String, A]): Map[String, A] =
  program.fold(
    _ => kvs,
    {
      case Put(key, value, next) => // help scalac past type erasure
        compilePure[A](next, kvs + (key -> value.asInstanceOf[A]))
      case g: Get[a, f] => // a bit more help for scalac
        compilePure(g.onResult(kvs(g.key).asInstanceOf[a]), kvs)
      case Delete(key, next) =>
        compilePure(next, kvs - key)
    })
```

(You can see that we are again running into some places where scala's
support for pattern matching is limited by the JVM's type erausre, but
it's not too hard to get around.)

```tut
val result: Map[String, Int] = compilePure(program, Map.empty)
```

## For the curious ones: what is Free in theory?

Mathematically-speaking, a *free monad* (at least in the programming
language context) is a construction that is left adjoint to a
forgetful functor whose domain is the category of Monads and whose
co-domain is the category of Endofunctors. Huh?

Concretely, **it is just a clever construction that allows us to build a
_very simple_ Monad from any _functor_**.

The above forgetful functor takes a `Monad` and:

 - forgets its *monadic* part (e.g. the `flatMap` function)
 - forgets its *applicative* part (e.g. the `pure` function)
 - finally keep the *functor* part (e.g. the `map` function)

By reversing all arrows to build the left-adjoint, we deduce that the
forgetful functor is basically a construction that:

 - takes a *functor*
 - adds the *applicative* part (e.g. `pure`)
 - adds the *monadic* behavior (e.g. `flatMap`)

In terms of implementation, to build a *monad* from a *functor* we use
the following classic inductive definition:

```scala
sealed abstract class Free[F[_], A]
case class Pure[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](a: F[Free[F, A]]) extends Free[F, A]
```

(_This generalizes the concept of fixed point functor_.)

In this representation:

 - `Pure` builds a `Free` instance from an `A` value (it _reifies_ the
   `pure` function)
 - `Suspend` build a new `Free` by applying `F` to a previous `Free`
   (it _reifies_ the `flatMap` function)

So a typical `Free` structure might look like:

```scala
Suspend(F(Suspend(F(Suspend(F(....(Pure(a))))))))
```

`Free` is a recursive structure. It uses `A` in `F[A]` as the
recursion "carrier" with a terminal element `Pure`.

From a computational point of view, `Free` recursive structure can be
seen as a sequence of operations.

 - `Pure` returns an `A` value and ends the entire computation.
- `Suspend` is a continuation; it suspends the current computation
  with the suspension functor `F` (which can represent a command for
  example) and hands control to the caller. `A` represents a value
  bound to this computation.

Please note this `Free` construction has the interesting quality of
_encoding_ the recursion on the heap instead of the stack as classic
function calls would. This provides the stack-safety we heard about
earlier, allowing very large `Free` structures to be evaluated safely.

### For the very curious ones

If you look at implementation in cats, you will see another member of
the `Free[_]` ADT:

```scala
sealed abstract case class Gosub[S[_], B]() extends Free[S, B] {
  type C
  val a: () => Free[S, C]
  val f: C => Free[S, B]
}
```

`Gosub` represents a call to a subroutine `a` and when `a` is
finished, it continues the computation by calling the function `f`
with the result of `a`.

It is actually an optimization of `Free` structure allowing to solve a
problem of quadratic complexity implied by very deep recursive Free
computations.

It is exactly the same problem as repeatedly appending to a `List[_]`.
As the sequence of operations becomes longer, the slower a `flatMap`
"through" the structure will be. With `Gosub`, `Free` becomes a
right-associated structure not subject to quadratic complexity.

## Future Work (TODO)

There are many remarkable uses of `Free[_]`. In the future, we will
include some here, such as:

 - Trampoline
 - Option
 - Iteratee
 - Source
 - etc...

We will also discuss the *Coyoneda Trick*.

## Credits

This article was written by
[Pascal Voitot](https://github.com/mandubian) and edited by other
members of the Cats community.
