# Free Applicative

API Documentation: @:api(cats.free.FreeApplicative)

`FreeApplicative`s are similar to `Free` (monads) in that they provide a nice way to represent
computations as data and are useful for building embedded DSLs (EDSLs). However, they differ
from `Free` in that the kinds of operations they support are limited, much like the distinction
between `Applicative` and `Monad`.

## Dependency

If you'd like to use cats' free applicative, you'll need to add a library dependency
for the `cats-free` module.

## Example
Consider building an EDSL for validating strings - to keep things simple we'll just have
a way to check a string is at least a certain size and to ensure the string contains numbers.

```scala mdoc:silent
sealed abstract class ValidationOp[A]
case class Size(size: Int) extends ValidationOp[Boolean]
case object HasNumber extends ValidationOp[Boolean]
```

Much like the `Free` monad tutorial, we use smart constructors to lift our algebra into the `FreeApplicative`.

```scala mdoc:silent
import cats.free.FreeApplicative
import cats.free.FreeApplicative.lift

type Validation[A] = FreeApplicative[ValidationOp, A]

def size(size: Int): Validation[Boolean] = lift(Size(size))

val hasNumber: Validation[Boolean] = lift(HasNumber)
```

Because a `FreeApplicative` only supports the operations of `Applicative`, we do not get the nicety
of a for-comprehension. We can however still use `Applicative` syntax provided by Cats.

```scala mdoc:silent
import cats.syntax.all._

val prog: Validation[Boolean] = (size(5), hasNumber).mapN { case (l, r) => l && r}
```

As it stands, our program is just an instance of a data structure - nothing has happened
at this point. To make our program useful we need to interpret it.

```scala mdoc:silent
import cats.Id
import cats.arrow.FunctionK
import cats.syntax.all._

// a function that takes a string as input
type FromString[A] = String => A

val compiler = new FunctionK[ValidationOp, FromString] {
  def apply[A](fa: ValidationOp[A]): FromString[A] = str =>
    fa match {
      case Size(size) => str.size >= size
      case HasNumber  => str.exists(c => "0123456789".contains(c))
    }
}
```

```scala mdoc
val validator = prog.foldMap[FromString](compiler)
validator("1234")
validator("12345")
```

## Differences from `Free`
So far everything we've been doing has been not much different from `Free` - we've built
an algebra and interpreted it. However, there are some things `FreeApplicative` can do that
`Free` cannot.

Recall a key distinction between the type classes `Applicative` and `Monad` - `Applicative`
captures the idea of independent computations, whereas `Monad` captures that of dependent
computations. Put differently `Applicative`s cannot branch based on the value of an existing/prior
computation. Therefore when using `Applicative`s, we must hand in all our data in one go.

In the context of `FreeApplicative`s, we can leverage this static knowledge in our interpreter.

### Parallelism
Because we have everything we need up front and know there can be no branching, we can easily
write a validator that validates in parallel.

```scala mdoc:silent
import cats.data.Kleisli
import cats.syntax.all._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

// recall Kleisli[Future, String, A] is the same as String => Future[A]
type ParValidator[A] = Kleisli[Future, String, A]

val parCompiler = new FunctionK[ValidationOp, ParValidator] {
  def apply[A](fa: ValidationOp[A]): ParValidator[A] = Kleisli { str =>
    fa match {
      case Size(size) => Future { str.size >= size }
      case HasNumber => Future { str.exists(c => "0123456789".contains(c)) }
    }
  }
}

val parValidator = prog.foldMap[ParValidator](parCompiler)
```

### Logging
We can also write an interpreter that simply creates a list of strings indicating the filters that
have been used - this could be useful for logging purposes. Note that we need not actually evaluate
the rules against a string for this, we simply need to map each rule to some identifier. Therefore
we can completely ignore the return type of the operation and return just a `List[String]` - the
`Const` data type is useful for this.

```scala mdoc:silent
import cats.data.Const
import cats.syntax.all._

type Log[A] = Const[List[String], A]

val logCompiler = new FunctionK[ValidationOp, Log] {
  def apply[A](fa: ValidationOp[A]): Log[A] = fa match {
    case Size(size) => Const(List(s"size >= $size"))
    case HasNumber => Const(List("has number"))
  }
}

def logValidation[A](validation: Validation[A]): List[String] =
  validation.foldMap[Log](logCompiler).getConst
```

```scala mdoc
logValidation(prog)
logValidation(size(5) *> hasNumber *> size(10))
logValidation((hasNumber, size(3)).mapN(_ || _))
```

### Why not both?
It is perhaps more plausible and useful to have both the actual validation function and the logging
strings. While we could easily compile our program twice, once for each interpreter as we have above,
we could also do it in one go - this would avoid multiple traversals of the same structure.

Another useful property `Applicative`s have over `Monad`s is that given two `Applicative`s `F[_]` and
`G[_]`, their product `type FG[A] = (F[A], G[A])` is also an `Applicative`. This is not true in the general
case for monads.

Therefore, we can write an interpreter that uses the product of the `ParValidator` and `Log` `Applicative`s
to interpret our program in one go. We can create this interpreter easily by using `FunctionK#and`.

```scala mdoc:silent
import cats.data.Tuple2K

type ValidateAndLog[A] = Tuple2K[ParValidator, Log, A]

val prodCompiler: FunctionK[ValidationOp, ValidateAndLog] = parCompiler and logCompiler

val prodValidation = prog.foldMap[ValidateAndLog](prodCompiler)
```

### The way FreeApplicative#foldMap works
Despite being an imperative loop, there is a functional intuition behind `FreeApplicative#foldMap`.

The new `FreeApplicative`'s `foldMap` is a sort of mutually-recursive function that operates on an argument stack and a 
function stack, where the argument stack has type `List[FreeApplicative[F, _]]` and the functions have type `List[Fn[G, _, _]]`.
`Fn[G[_, _]]` contains a function to be `Ap`'d that has already been translated to the target `Applicative`,
as well as the number of functions that were `Ap`'d immediately subsequently to it.

#### Main re-association loop
Pull an argument out of the stack, eagerly remove right-associated `Ap` nodes, by looping on the right and 
adding the `Ap` nodes' arguments on the left to the argument stack; at the end, pushes a single function to the 
function stack of the applied functions, the rest of which will be pushed in this loop in later iterations. 
Once all of the `Ap` nodes on the right are removed, the loop resets to deal with the ones on the left.

Here's an example `FreeApplicative` value to demonstrate the loop's function, at the end of every iteration.
Every node in the tree is annotated with an identifying number and the concrete type of the node
(A -> `Ap`, L -> `Lift`, P -> `Pure`), and an apostrophe to denote where `argF` (the current argument) currently
points; as well the argument and function branches off `Ap` nodes are explicitly denoted.

```
==> begin.
           '1A
           /  \
       arg/    \fun
         /      \
        /        \
       2A        3A
   arg/  \fun arg/  \fun
     /    \     /    \
    4L    5P   6L     7L

args: Nil
functions: Nil
==> loop.

            1A
           /  \
       arg/    \fun
         /      \
        /        \
       2A        '3A
   arg/  \fun arg/  \fun
     /    \     /    \
    4L    5P   6L    7L

args: 2A :: Nil
functions: Nil
==> loop.

            1A
           /  \
       arg/    \fun
         /      \
        /        \
       2A         3A
   arg/  \fun arg/  \fun
     /    \     /    \
    4L    5P   6L   '7L

args: 6L :: 2A :: Nil
functions: Fn(gab = foldArg(7L), argc = 2) :: Nil
==> finished.
```

At the end of the loop the entire right branch of `Ap`s under `argF` has been peeled off into a single curried function,
all of the arguments to that function are on the argument stack and that function itself is on the function stack,
annotated with the amount of arguments it takes.

#### Function application loop
Once `argF` isn't an `Ap` node, a loop runs which pulls functions from the stack until it reaches a curried function,
in which case it applies the function to `argF` transformed into a `G[Any]` value, and pushes the resulting function
back to the function stack, before returning to the main loop.

I'll continue the example from before here:
```
==> loop.
            1A
           /  \
       arg/    \fun
         /      \
        /        \
       2A         3A
   arg/  \fun arg/  \fun
     /    \     /    \
    4L    5P  '6L    7L

args: 2A :: Nil
functions: Fn(gab = foldArg(7L) ap foldArg(6L), argc = 1) :: Nil
==> finished.
```

At the end of this loop every function on the top of the function stack with `length == 1` (not curried)
has been applied to a single argument from the argument stack, and the first curried function (`length != 1`)
on the stack has been applied to a single argument from the argument stack.
The reason we can't keep applying the curried function to arguments is that the node on top of the argument
stack *must* be an `Ap` node if the function is curried, so we can't translate it directly to `G[_]`.

Once the last function has been applied to the last argument, the fold has finished and the result is returned.

## References
Deeper explanations can be found in this paper [Free Applicative Functors by Paolo Capriotti](http://www.paolocapriotti.com/assets/applicative.pdf)
