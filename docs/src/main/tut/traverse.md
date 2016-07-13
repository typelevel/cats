---
layout: default
title:  "Traverse"
section: "typeclasses"
source: "core/src/main/scala/cats/Traverse.scala"
scaladoc: "#cats.Traverse"
---
# Traverse
In functional programming it is very common to encode "effects" as data types - common effects
include `Option` for possibly missing values, `Xor` and `Validated` for possible errors, and
`Future` for asynchronous computations.

These effects tend to show up in functions working on a single piece of data - for instance
parsing a single `String` into an `Int`, validating a login, or asynchronously fetching website
information for a user.

```tut:silent
import cats.data.Xor
import scala.concurrent.Future

def parseInt(s: String): Option[Int] = ???

trait SecurityError
trait Credentials

def validateLogin(cred: Credentials): Xor[SecurityError, Unit] = ???

trait Profile
trait User

def userInfo(user: User): Future[Profile] = ???
```

Each function asks only for the data it actually needs; in the case of `userInfo`, a single `User`. We
certainly could write one that takes a `List[User]` and fetch profile for all of them, would be a bit strange.
If we just wanted to fetch the profile of just one user, we would either have to wrap it in a `List` or write
a separate function that takes in a single user anyways. More fundamentally, functional programming is about
building lots of small, independent pieces and composing them to make larger and larger pieces - does this
hold true in this case?

Given just `User => Future[Profile]`, what should we do if we want to fetch profiles for a `List[User]`?
We could try familiar combinators like `map`.

```tut:silent
def profilesFor(users: List[User]): List[Future[Profile]] = users.map(userInfo)
```

Note the return type `List[Future[Profile]]`. This makes sense given the type signatures, but seems unwieldy.
We now have a list of asynchronous values, and to work with those values we must then use the combinators on
`Future` for every single one. It would be nicer instead if we could get the aggregate result in a single
`Future`, say a `Future[List[Profile]]`.

As it turns out, the `Future` companion object has a `traverse` method on it. However, that method is
specialized to standard library collections and `Future`s - there exists a much more generalized form
that would allow us to parse a `List[String]` or validate credentials for a `List[User]`.

Enter `Traverse`.

## The type class
At center stage of `Traverse` is the `traverse` method.

```scala
trait Traverse[F[_]] {
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
}
```

In our above example, `F` is `List`, and `G` is `Option`, `Xor`, or `Future`. For the profile example,
`traverse` says given a `List[User]` and a function `User => Future[Profile]`, it can give you a
`Future[List[Profile]]`.

Abstracting away the `G` (still imagining `F` to be `List`), `traverse` says given a collection of data,
and a function that takes a piece of data and returns an effectful value, it will traverse the collection,
applying the function and aggregating the effectful values (in a `List`) as it goes.

In the most general form, `F[_]` is some sort of context which may contain a value (or several). While
`List` tends to be among the most general cases, there also exist `Traverse` instances for `Option`,
`Xor`, and `Validated` (among others).

## Choose your effect
The type signature of `Traverse` appears highly abstract, and indeed it is - what `traverse` does as it
walks the `F[A]` depends on the effect of the function. Let's see some examples where `F` is taken to be
`List`.

Note in the following code snippet we are using `traverseU` instead of `traverse`.
`traverseU` is for all intents and purposes the same as `traverse`, but with some
[type-level trickery](http://typelevel.org/blog/2013/09/11/using-scalaz-Unapply.html)
to allow it to infer the `Applicative[Xor[A, ?]]` and `Applicative[Validated[A, ?]]`
instances - `scalac` has issues inferring the instances for data types that do not
trivially satisfy the `F[_]` shape required by `Applicative`.

```tut:silent
import cats.Semigroup
import cats.data.{NonEmptyList, OneAnd, Validated, ValidatedNel, Xor}
import cats.implicits._

def parseIntXor(s: String): Xor[NumberFormatException, Int] =
  Xor.catchOnly[NumberFormatException](s.toInt)

def parseIntValidated(s: String): ValidatedNel[NumberFormatException, Int] =
  Validated.catchOnly[NumberFormatException](s.toInt).toValidatedNel
```

Examples.

```tut:book
val x1 = List("1", "2", "3").traverseU(parseIntXor)
val x2 = List("1", "abc", "3").traverseU(parseIntXor)
val x3 = List("1", "abc", "def").traverseU(parseIntXor)

val v1 = List("1", "2", "3").traverseU(parseIntValidated)
val v2 = List("1", "abc", "3").traverseU(parseIntValidated)
val v3 = List("1", "abc", "def").traverseU(parseIntValidated)
```

Notice that in the `Xor` case, should any string fail to parse the entire traversal
is considered a failure. Moreover, once it hits its first bad parse, it will not
attempt to parse any others down the line (similar behavior would be found with
using `Option` as the effect). Contrast this with `Validated` where even
if one bad parse is hit, it will continue trying to parse the others, accumulating
any and all errors as it goes. The behavior of traversal is closely tied with the
`Applicative` behavior of the data type.

Going back to our `Future` example, we can write an `Applicative` instance for
`Future` that runs each `Future` concurrently. Then when we traverse a `List[A]`
with an `A => Future[B]`, we can imagine the traversal as a scatter-gather.
Each `A` creates a concurrent computation that will produce a `B` (the scatter),
and as the `Future`s complete they will be gathered back into a `List`.

### Playing with `Reader`
Another interesting effect we can use is `Reader`. Recall that a `Reader[E, A]` is
a type alias for `Kleisli[Id, E, A]` which is a wrapper around `E => A`.

If we fix `E` to be some sort of environment or configuration, we can use the
`Reader` applicative in our traverse.

```tut:silent
import cats.data.Reader

trait Context
trait Topic
trait Result

type Job[A] = Reader[Context, A]

def processTopic(topic: Topic): Job[Result] = ???
```

We can imagine we have a data pipeline that processes a bunch of data, each piece of data
being categorized by a topic. Given a specific topic, we produce a `Job` that processes
that topic. (Note that since a `Job` is just a `Reader`/`Kleisli`, one could write many small
`Job`s and compose them together into one `Job` that is used/returned by `processTopic`.)

Corresponding to our bunches of data are bunches of topics, a `List[Topic]` if you will.
Since `Reader` has an `Applicative` instance, we can `traverse` over this list with `processTopic`.

```tut:silent
def processTopics(topics: List[Topic]): Job[List[Result]] = 
  topics.traverse(processTopic)
```

Note the nice return type - `Job[List[Result]]`. We now have one aggregate `Job` that when run,
will go through each topic and run the topic-specific job, collecting results as it goes.
We say "when run" because a `Job` is some function that requires a `Context` before producing
the value we want.

One example of a "context" can be found in the [Spark](http://spark.apache.org/) project. In
Spark, information needed to run a Spark job (where the master node is, memory allocated, etc.)
resides in a `SparkContext`. Going back to the above example, we can see how one may define
topic-specific Spark jobs (`type Job[A] = Reader[SparkContext, A]`) and then run several
Spark jobs on a collection of topics via `traverse`. We then get back a `Job[List[Result]]`,
which is equivalent to `SparkContext => List[Result]`. When finally passed a `SparkContext`,
we can run the job and get our results back.

Moreover, the fact that our aggregate job is not tied to any specific `SparkContext` allows us
to pass in a `SparkContext` pointing to a production cluster, or (using the exact same job) pass
in a test `SparkContext` that just runs locally across threads. This makes testing our large
job nice and easy.

Finally, this encoding ensures that all the jobs for each topic run on the exact same cluster.
At no point do we manually pass in or thread a `SparkContext` through - that is taken care for us
by the (applicative) effect of `Reader` and therefore by `traverse`.

## Sequencing
Sometimes you may find yourself with a collection of data, each of which is already in an effect,
for instance a `List[Option[A]]`. To make this easier to work with, you want a `Option[List[A]]`.
Given `Option` has an `Applicative` instance, we can traverse over the list with the identity function.

```tut:book
import cats.implicits._
val l1 = List(Option(1), Option(2), Option(3)).traverse(identity)
val l2 = List(Option(1), None, Option(3)).traverse(identity)
```

`Traverse` provides a convenience method `sequence` that does exactly this.

```tut:book
val l1 = List(Option(1), Option(2), Option(3)).sequence
val l2 = List(Option(1), None, Option(3)).sequence
```

## Traversing for effect
Sometimes our effectful functions return a `Unit` value in cases where there is no interesting value
to return (e.g. writing to some sort of store).

```tut:silent
trait Data
def writeToStore(data: Data): Future[Unit] = ???
```

If we traverse using this, we end up with a funny type.

```tut:silent
import cats.implicits._
import scala.concurrent.ExecutionContext.Implicits.global

def writeManyToStore(data: List[Data]) =
  data.traverse(writeToStore)
```

We end up with a `Future[List[Unit]]`! A `List[Unit]` is not of any use to us, and communicates the
same amount of information as a single `Unit` does.

Traversing solely for the sake of the effect (ignoring any values that may be produced, `Unit` or otherwise)
is common, so `Foldable` (superclass of `Traverse`) provides `traverse_` and `sequence_` methods that do the
same thing as `traverse` and `sequence` but ignores any value produced along the way, returning `Unit` at the end.

```tut:silent
import cats.implicits._

def writeManyToStore(data: List[Data]) = 
  data.traverse_(writeToStore)

// Int values are ignored with traverse_
def writeToStoreV2(data: Data): Future[Int] = 
  ???

def writeManyToStoreV2(data: List[Data]) = 
  data.traverse_(writeToStoreV2)
```
