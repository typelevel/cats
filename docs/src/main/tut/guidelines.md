---
layout: page
title:  "Guidelines"
section: "guidelines"
position: 70
---

# Guidelines

All guidelines in cats should have clear justifications. There is no room for tribal wisdom in a simple library.

## Syntax

### <a id="implicit-syntax-conversions" href="#implicit-syntax-conversions"></a> Composing Implicit Conversions in Traits

Implicit syntax conversions provided in publicly-exposed traits should be marked final
so that any composition of the traits provides conversions that can all be inlined.

### <a id="ops-classes" href="#ops-classes"></a> Ops Classes

Ops classes should be marked final and extend AnyVal, to take full advantage of inlining and prevent unnecessary allocations.

The most notable exception is the case where all of the ops in the class are provided by zero-cost macros anyway,
for example with Simulacrum.

### <a id="partially-applied-type-params" href="#partially-applied-type-params"></a> Partially-Applied Type

In Scala, when there are multiple type parameters in a function, either scalac infers all type parameters or the user has to
specify all of them. Often we have functions where there are one or more types that are inferable but not all of them. For example, there is helper function in `OptionT` that creates an `OptionT[F, A]` from an `A`. It could be written as:

```tut:silent
import cats._
import cats.implicits._
import cats.data.OptionT
```
```tut:book
def pure[F[_], A](a: A)(implicit F: Applicative[F]): OptionT[F, A] =
  OptionT(F.pure(Some(a)))


pure[List, Int](1)
```

Note that the type `A` should've been given by the `a: A` argument, but since scalac cannot infer `F[_]`, the user still has to specify all type params.
In cats, we use a technique described in
 Rob Norris’s [Kinda-Curried Type Parameters](https://tpolecat.github.io/2015/07/30/infer.html) to overcome this restriction of scala inference. Here is a version of the `pure` using this technique in cats.

```scala
package cats.data

object OptionT {

  private[data] final class PurePartiallyApplied[F[_]](val dummy: Boolean = true ) extends AnyVal {
    def apply[A](value: A)(implicit F: Applicative[F]): OptionT[F, A] =
      OptionT(F.pure(Some(value)))
  }

  def pure[F[_]]: PurePartiallyApplied[F] = new PurePartiallyApplied[F]
}
```

We introduced an intermediate or, as the name suggested, type parameter partially applied type `PurePartiallyApplied` to divide the function into two steps: the first step is a construction of the partially applied type, for which the type `F[_]` is given by the user; the second step is the `apply` method inside partially applied type, for which the `A` can be inferred from the argument. Now we can write:
```tut:book
OptionT.pure[List](1)
```

The user doesn't need to specify the type `A` which is given by the parameter.

You probably noticed that there is a `val dummy: Boolean` in the `PurePartiallyApplied` class. This is a trick we used
to make this intermediate class a [Value Class](http://docs.scala-lang.org/overviews/core/value-classes.html) so that there is no cost of allocation, i.e. at runtime, it doesn't create an instance of `PurePartiallyApplied`. We also hide this partially applied class by making it package private and placing it inside an object.

### <a id="implicit-naming" href="#implicit-naming"></a> Implicit naming

In a widely-used library it's important to minimize the chance that the names of implicits will be used by others and
therefore name our implicits according to the following rules:

- Implicits should start with "cats" followed by the package name (where the instance is defined).
- If the package contains `instances` leave `instances` out.
- The type and the type class should be mentioned in the name.
- If the instance is for multiple type classes, use `InstancesFor` instead of a type class name.
- If the instance is for a standard library type add `Std` after the package. i.e. `catsStdShowForVector` and `catsKernelStdGroupForTuple`.

As an example, an implicit instance of `Monoid` for `List` defined in the package `Kernel` should be named `catsKernelMonoidForList`.

This rule is relatively flexible. Use what you see appropriate. The goal is to maintain uniqueness and avoid conflicts.



### <a id="implicit-priority" href="#implicit-priority"></a> Implicit instance priority

When there are multiple instances provided implicitly, if the type class of them are in the same inheritance hierarchy, 
the instances need to be separated out into different abstract class/traits so that they don't conflict with each other. The names of these abstract classes/traits should be numbered with a priority with 0 being the highest priority. The abstract classes/trait
with higher priority inherits from the ones with lower priority. The most specific (whose type class is the lowest in the hierarchy) instance should be placed in the abstract class/ trait with the highest priority.  Here is an example. 

```scala
@typeclass 
trait Functor[F[_]]

@typeclass
trait Monad[F[_]] extends Functor

...
object Kleisli extends KleisliInstance0 

abstract class KleisliInstance0 extends KleisliInstance1 {
  implicit def catsDataMonadForKleisli[F[_], A]: Monad[Kleisli[F, A, ?]] = ...
}

abstract class KleisliInstance1 {
  implicit def catsDataFunctorForKleisli[F[_], A]: Functor[Kleisli[F, A, ?]] = ...
}
```

#### TODO:
Once we drop 2.10 support, AnyVal-extending class constructor parameters can be marked as private.
