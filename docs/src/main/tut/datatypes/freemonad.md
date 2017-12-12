---
layout: docs
title:  "FreeMonads"
section: "data"
source: "free/src/main/scala/cats/free/Free.scala"
scaladoc: "#cats.free.Free"
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

If you'd like to use cats' free monad, you'll need to add a library dependency
for the `cats-free` module.

A good way to get a sense for how *free monads* work is to see them in
action. The next section uses `Free[_]` to create an embedded DSL
(Domain Specific Language).

If you're interested in the theory behind *free monads*, the
[What is Free in theory?](#what-is-free-in-theory) section discusses free monads
in terms of category theory.

### Study your topic

Let's imagine that we want to create a DSL for a key-value store. We
want to be able to do three things with keys:

 - *put* a `value` into the store, associated with its `key`.
 - *get* a `value` from the store given its `key`.
 - *delete* a `value` from the store given its `key`.

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

```tut:silent
sealed trait KVStoreA[A]
case class Put[T](key: String, value: T) extends KVStoreA[Unit]
case class Get[T](key: String) extends KVStoreA[Option[T]]
case class Delete(key: String) extends KVStoreA[Unit]
```

### Free your ADT

There are five basic steps to "freeing" the ADT:

1. Create a type based on `Free[_]` and `KVStoreA[_]`.
2. Create smart constructors for `KVStore[_]` using `liftF`.
3. Build a program out of key-value DSL operations.
4. Build a compiler for programs of DSL operations.
5. Execute our compiled program.

#### 1. Create a `Free` type based on your ADT

```tut:silent
import cats.free.Free

type KVStore[A] = Free[KVStoreA, A]
```

#### 2. Create smart constructors using `liftF`

These methods will make working with our DSL a lot nicer, and will
lift `KVStoreA[_]` values into our `KVStore[_]` monad (note the
missing "A" in the second type).

```tut:silent
import cats.free.Free.liftF

// Put returns nothing (i.e. Unit).
def put[T](key: String, value: T): KVStore[Unit] =
  liftF[KVStoreA, Unit](Put[T](key, value))

// Get returns a T value.
def get[T](key: String): KVStore[Option[T]] =
  liftF[KVStoreA, Option[T]](Get[T](key))

// Delete returns nothing (i.e. Unit).
def delete(key: String): KVStore[Unit] =
  liftF(Delete(key))

// Update composes get and set, and returns nothing.
def update[T](key: String, f: T => T): KVStore[Unit] =
  for {
    vMaybe <- get[T](key)
    _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
  } yield ()
```

#### 3. Build a program

Now that we can construct `KVStore[_]` values we can use our DSL to
write "programs" using a *for-comprehension*:

```tut:silent
def program: KVStore[Option[Int]] =
  for {
    _ <- put("wild-cats", 2)
    _ <- update[Int]("wild-cats", (_ + 12))
    _ <- put("tame-cats", 5)
    n <- get[Int]("wild-cats")
    _ <- delete("tame-cats")
  } yield n
```

This looks like a monadic flow. However, it just builds a recursive
data structure representing the sequence of operations.

#### 4. Write a compiler for your program

As you may have understood now, `Free[_]` is used to create an embedded
DSL. By itself, this DSL only represents a sequence of operations
(defined by a recursive data structure); it doesn't produce anything.

`Free[_]` is a programming language inside your programming language!

**So, like any other programming language, we need to compile our
  abstract language into an _effective_ language and then run it.**

To do this, we will use a *natural transformation* between type
containers.  Natural transformations go between types like `F[_]` and
`G[_]` (this particular transformation would be written as
`FunctionK[F,G]` or as done here using the symbolic
alternative as `F ~> G`).

In our case, we will use a simple mutable map to represent our key
value store:

```tut:silent
import cats.arrow.FunctionK
import cats.{Id, ~>}
import scala.collection.mutable

// the program will crash if a key is not found,
// or if a type is incorrectly specified.
def impureCompiler: KVStoreA ~> Id  =
  new (KVStoreA ~> Id) {

    // a very simple (and imprecise) key-value store
    val kvs = mutable.Map.empty[String, Any]

    def apply[A](fa: KVStoreA[A]): Id[A] =
      fa match {
        case Put(key, value) =>
          println(s"put($key, $value)")
          kvs(key) = value
          ()
        case Get(key) =>
          println(s"get($key)")
          kvs.get(key).map(_.asInstanceOf[A])
        case Delete(key) =>
          println(s"delete($key)")
          kvs.remove(key)
          ()
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
 - `Either[E, ?]` to support failure
 - a pseudo-random monad to support non-determinism
 - and so on...

#### 5. Run your program

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
final def foldMap[M[_]](f: FunctionK[S,M])(M: Monad[M]): M[A] = ...
```

`M` must be a `Monad` to be flattenable (the famous monoid aspect
under `Monad`). As `Id` is a `Monad`, we can use `foldMap`.

To run your `Free` with previous `impureCompiler`:

```tut:book
val result: Option[Int] = program.foldMap(impureCompiler)
```

An important aspect of `foldMap` is its **stack-safety**. It evaluates
each step of computation on the stack then unstack and restart. This
process is known as trampolining.

As long as your natural transformation is stack-safe, `foldMap` will
never overflow your stack.  Trampolining is heap-intensive but
stack-safety provides the reliability required to use `Free[_]` for
data-intensive tasks, as well as infinite processes such as streams.

#### 6. Use a pure compiler (optional)

The previous examples used an effectful natural transformation. This
works, but you might prefer folding your `Free` in a "purer" way. The
[State](state.html) data structure can be used to keep track of the program
state in an immutable map, avoiding mutation altogether.

```tut:silent
import cats.data.State

type KVStoreState[A] = State[Map[String, Any], A]
val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {
  def apply[A](fa: KVStoreA[A]): KVStoreState[A] =
    fa match {
      case Put(key, value) => State.modify(_.updated(key, value))
      case Get(key) =>
        State.inspect(_.get(key).map(_.asInstanceOf[A]))
      case Delete(key) => State.modify(_ - key)
    }
}
```

(You can see that we are again running into some places where Scala's
support for pattern matching is limited by the JVM's type erasure, but
it's not too hard to get around.)

```tut:book
val result: (Map[String, Any], Option[Int]) = program.foldMap(pureCompiler).run(Map.empty).value
```

## Composing Free monads ADTs.

Real world applications often time combine different algebras.
The injection type class described by Swierstra in [Data types à la carte](http://www.staff.science.uu.nl/~swier004/publications/2008-jfp.pdf)
lets us compose different algebras in the context of `Free`.

Let's see a trivial example of unrelated ADT's getting composed as a `EitherK` that can form a more complex program.

```tut:silent
import cats.data.EitherK
import cats.free.Free
import cats.{Id, InjectK, ~>}
import scala.collection.mutable.ListBuffer
```

```tut:silent
/* Handles user interaction */
sealed trait Interact[A]
case class Ask(prompt: String) extends Interact[String]
case class Tell(msg: String) extends Interact[Unit]

/* Represents persistence operations */
sealed trait DataOp[A]
case class AddCat(a: String) extends DataOp[Unit]
case class GetAllCats() extends DataOp[List[String]]
```

Once the ADTs are defined we can formally state that a `Free` program is the EitherK of it's Algebras.

```tut:silent
type CatsApp[A] = EitherK[DataOp, Interact, A]
```

In order to take advantage of monadic composition we use smart constructors to lift our Algebra to the `Free` context.

```tut:silent
class Interacts[F[_]](implicit I: InjectK[Interact, F]) {
  def tell(msg: String): Free[F, Unit] = Free.inject[Interact, F](Tell(msg))
  def ask(prompt: String): Free[F, String] = Free.inject[Interact, F](Ask(prompt))
}

object Interacts {
  implicit def interacts[F[_]](implicit I: InjectK[Interact, F]): Interacts[F] = new Interacts[F]
}

class DataSource[F[_]](implicit I: InjectK[DataOp, F]) {
  def addCat(a: String): Free[F, Unit] = Free.inject[DataOp, F](AddCat(a))
  def getAllCats: Free[F, List[String]] = Free.inject[DataOp, F](GetAllCats())
}

object DataSource {
  implicit def dataSource[F[_]](implicit I: InjectK[DataOp, F]): DataSource[F] = new DataSource[F]
}
```

ADTs are now easily composed and trivially intertwined inside monadic contexts.

```tut:silent
def program(implicit I : Interacts[CatsApp], D : DataSource[CatsApp]): Free[CatsApp, Unit] = {

  import I._, D._

  for {
    cat <- ask("What's the kitty's name?")
    _ <- addCat(cat)
    cats <- getAllCats
    _ <- tell(cats.toString)
  } yield ()
}
```

Finally we write one interpreter per ADT and combine them with a `FunctionK` to `EitherK` so they can be
compiled and applied to our `Free` program.

```tut:invisible
def readLine(): String = "snuggles"
```

```tut:silent
object ConsoleCatsInterpreter extends (Interact ~> Id) {
  def apply[A](i: Interact[A]) = i match {
    case Ask(prompt) =>
      println(prompt)
      readLine()
    case Tell(msg) =>
      println(msg)
  }
}

object InMemoryDatasourceInterpreter extends (DataOp ~> Id) {

  private[this] val memDataSet = new ListBuffer[String]

  def apply[A](fa: DataOp[A]) = fa match {
    case AddCat(a) => memDataSet.append(a); ()
    case GetAllCats() => memDataSet.toList
  }
}

val interpreter: CatsApp ~> Id = InMemoryDatasourceInterpreter or ConsoleCatsInterpreter
```

Now if we run our program and type in "snuggles" when prompted, we see something like this:

```tut:silent
import DataSource._, Interacts._
```

```tut:book
val evaled: Unit = program.foldMap(interpreter)
```

## <a name="what-is-free-in-theory"></a>For the curious ones: what is Free in theory?

Mathematically-speaking, a *free monad* (at least in the programming
language context) is a construction that is left adjoint to a
forgetful functor whose domain is the category of Monads and whose
co-domain is the category of Endofunctors. Huh?

Concretely, **it is just a clever construction that allows us to build a
_very simple_ Monad from any _functor_**.

The above forgetful functor takes a `Monad` and:

 - forgets its *monadic* part (e.g. the `flatMap` function)
 - forgets its *pointed* part (e.g. the `pure` function)
 - finally keeps the *functor* part (e.g. the `map` function)

By reversing all arrows to build the left-adjoint, we deduce that the
free monad is basically a construction that:

 - takes a *functor*
 - adds the *pointed* part (e.g. `pure`)
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
 - `Suspend` builds a new `Free` by applying `F` to a previous `Free`
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
case class FlatMapped[S[_], B, C](c: Free[S, C], f: C => Free[S, B]) extends Free[S, B]
```

`FlatMapped` represents a call to a subroutine `c` and when `c` is
finished, it continues the computation by calling the function `f`
with the result of `c`.

It is actually an optimization of `Free` structure allowing to solve a
problem of quadratic complexity implied by very deep recursive `Free`
computations.

It is exactly the same problem as repeatedly appending to a `List[_]`.
As the sequence of operations becomes longer, the slower a `flatMap`
"through" the structure will be. With `FlatMapped`, `Free` becomes a
right-associated structure not subject to quadratic complexity.

## FreeT

Often times we want to interleave the syntax tree when building a Free monad
with some other effect not declared as part of the ADT.
FreeT solves this problem by allowing us to mix building steps of the AST
with calling action in other base monad.

In the following example a basic console application is shown.
When the user inputs some text we use a separate `State` monad to track what the user
typed.

As we can observe in this case `FreeT` offers us a the alternative to delegate denotations to `State`
monad with stronger equational guarantees than if we were emulating the `State` ops in our own ADT.

```tut:book
import cats.free._
import cats._
import cats.data._

/* A base ADT for the user interaction without state semantics */
sealed abstract class Teletype[A] extends Product with Serializable
final case class WriteLine(line : String) extends Teletype[Unit]
final case class ReadLine(prompt : String) extends Teletype[String]

type TeletypeT[M[_], A] = FreeT[Teletype, M, A]
type Log = List[String]

type TeletypeState[A] = State[List[String], A]

/** Teletype smart constructors */
object TeletypeOps {
  def writeLine(line : String) : TeletypeT[TeletypeState, Unit] =
	FreeT.liftF[Teletype, TeletypeState, Unit](WriteLine(line))
  def readLine(prompt : String) : TeletypeT[TeletypeState, String] =
	FreeT.liftF[Teletype, TeletypeState, String](ReadLine(prompt))
  def log(s : String) : TeletypeT[TeletypeState, Unit] =
	FreeT.liftT[Teletype, TeletypeState, Unit](State.modify(s :: _))
}

def program : TeletypeT[TeletypeState, Unit] = {
  for {
	userSaid <- TeletypeOps.readLine("what's up?!")
	_ <- TeletypeOps.log(s"user said : $userSaid")
	_ <- TeletypeOps.writeLine("thanks, see you soon!")
  } yield ()
}

def interpreter = new (Teletype ~> TeletypeState) {
  def apply[A](fa : Teletype[A]) : TeletypeState[A] = {
	fa match {
	  case ReadLine(prompt) =>
		println(prompt)
		val userInput = "hanging in here" //scala.io.StdIn.readLine()
		StateT.pure[Eval, List[String], A](userInput)
	  case WriteLine(line) =>
		StateT.pure[Eval, List[String], A](println(line))
	}
  }
}

import TeletypeOps._

val state = program.foldMap(interpreter)
val initialState = Nil
val (stored, _) = state.run(initialState).value
```

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
