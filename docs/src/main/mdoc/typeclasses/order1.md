---
layout: docs
title:  "Eq1, PartialOrder1, and Order1"
section: "typeclasses"
source: "kernel/src/main/scala/cats/kernel/Order1.scala"
scaladoc: "#cats.kernel.Order1"
---

# Overview

`Order1` lifts the `Order` typeclass to unary type constructors, e.g. types of the form `F[_]`. It is defined by the single method `liftCompare`.

```scala
def liftCompare[A, B](compare: (A, B) => Int, x: F[A], y: F[B]): Int
```

`Order1` extends `PartialOrder1` which in turn extends `Eq1`, with each lifting their corresponding instance into a unary type constructor.

This can be used to express how to construct instances of these classes for higher kinded types more succinctly than using the base classes. The utility of these classes is also found when using them as constraints, particularly in conjunction with higher kinded data.

To demonstrate understand the utility of `Order1` it is useful to talk about it in concert with `PartialOrder1` and `Eq1`. With that in mind, let us first examine `Eq1` in isolation and we'll come back to `Order1` after.

# Eq1

Eq1 lifts the Eq typeclass to unary type constructors, e.g. types of the form `F[_]`. It is defined by the single method `liftEq`.

```scala
def liftEq[A, B](compare: (A, B) => Boolean, x: F[A], y: F[B]): Boolean
```

Side note: A common question when looking at this definition is why are we using `A` and `B` rather than just `A`, e.g. `compare: (A, A) => Boolean`. The reason for this permitting the arguments to be different is that this just happens to be the most general definition of this function, though in practice in almost all useful cases `A` and `B` will both be the same type. If this doesn't makes sense yet, don't worry about it for now, just think of `A` and `B` as both being `A`.

Here is how we could define an instance for `Option`.

```scala mdoc:silent
import cats.kernel.Eq1

implicit val instance: Eq1[Option] =
  new Eq1[Option] {
    override def liftEq[A, B](compare: (A, B) => Boolean, x: Option[A], y: Option[B]): Boolean =
      (x, y) match {
        case (Some(x), Some(y)) =>
          compare(x, y)
        case (None, None) =>
          true
        case _ =>
          false
      }
}
```

This is similar to defining an `Eq` instance for `Option` in this manner.

```scala mdoc:compile-only
import cats.kernel.Eq

implicit def instance[A: Eq]: Eq[Option[A]] =
    Eq.instance{
      case (Some(x), Some(y)) =>
        Eq[A].eqv(x, y)
      case (None, None) =>
        true
      case _ =>
        false
    }
```

The primary difference in these definitions is that the classical `Eq` instance requires that we constraint `A` to also have an `Eq` instance, but `Eq1` does not. Now, having only an instance of `Eq1[Option]` is less useful than an `Eq[Option[A]]` for some `A` because if we actually want to use our `Eq1[Option]` for any useful comparisons, we still have to provide a function `(A, B) => Boolean` for comparing `Option[A]` and `Option[B]`. We can derive an `Eq[Option[A]]` from an `Eq1[Option]` for any `A` which has an `Eq` instance. For example,

```scala mdoc:silent
cats.kernel.Eq1[Option]
cats.kernel.Eq[Int]
cats.kernel.Eq[Option[Int]]
```

However, this is similar to just defining an `Eq[Option[A]]` instance in the classical manner. Which begs the question, "Why would we ever want to use `Eq1`?". There are two reasons that the higher kinded classes such as `Eq1` have greater utility. The first relates to using `Eq1[Option]` as a constraint, particuarlly in higher kinded data (the second we will discuss later). Consider the following data type definition for a person.

```scala mdoc:reset
final case class Person[F[_]](
  name: F[String],
  age: F[Int],
  height: F[Double]
)
```

Perhaps our canonical values of `F` are `Id` and `Option`. We use `Option` when we are building up `Person` and then once we know all values are present, we switch to `Id`.

```scala mdoc
import cats.Id

def toId(value: Person[Option]): Option[Person[Id]] =
  for {
    name <- value.name
    age <- value.age
    height <- value.height
  } yield Person[Id](name, age, height)
```

Ideally we would like to have an `Eq` instance for this type, but defining one using just `Eq` is a bit cumbersome, particularly in the type signature.

```scala
def eqInstanceForPerson[F[_]](implicit A: Eq[F[String]], B: Eq[F[Int]], C: Eq[F[Double]]): Eq[Person[F]] =
  Eq.instance{
    case (x, y) =>
      x.name === y.name &&
      x.age === y.age &&
      x.height === y.height
  }
```

The reason we have to do this is that we require an `Eq` for _each_ concrete type `F[A]` for each possible `F[A]` in our data type. An alternative approach might be to just define `Eq` instances for the types of `F` we expect to use.

```scala
def eqInstanceForPersonOption: Eq[Person[Option]] = ???
def eqInstanceForPersonId: Eq[Person[Id]] = ???
```

This is better in that we don't have to express N number of constraints for each permutation of `F[A]`, but restricts our instances in ways we would rather avoid. Who is to say that we won't want to use `Either[Throwable, *]` in the future as our `F[_]` value.

However if we demand that our `F` has an `Eq1` instance, the situation improves quite a bit.

```scala mdoc
import cats._
import cats.syntax.all._

implicit def eqInstanceForPerson[F[_]: Eq1]: Eq[Person[F]] =
  Eq.instance{
    case (x, y) =>
      x.name === y.name &&
      x.age === y.age &&
      x.height === y.height
  }
```

Recall that `Eq1` can derive an `Eq[F[A]]` for any `A` which has an `Eq`. In this way our constraint of `Eq1[F]` is actually more precise to what we are trying to express. We already know that `String`, `Int`, and `Double` have `Eq` instances, what we really want to express is that the instance is valid for any `F[_]` such that we can create an `Eq[F[A]]` if we already know how to compare values of type `A` and this is exactly what `Eq1` let's us do.

# Order1

Now that we've looked at how `Eq1` can reduce the number of constraints required to write instances for higher kinded data, let's examine how it can reduce the number _instances_ we need to write for many `F` types.

As a motivating example, let us consider writing an `Order` instance for `Option`.

```scala mdoc:reset
import cats._
import cats.syntax.all._

def orderInstanceForOption[A: Order]: Order[Option[A]] =
  Order.from{
    case (Some(x), Some(y)) =>
      x.compare(y)
    case (None, None) =>
      0
    case (Some(_), _) =>
      1
    case _ =>
      -1
  }
```

At first glance this seems perfectly straight forward and fine. Now let's try to use this instance to get an `Eq` for a few different types of `Option`.

```scala mdoc
(orderInstanceForOption[Int]: Eq[Option[Int]])
```

That works just fine.

```scala mdoc:fail
(orderInstanceForOption[Map[String, String]]: Eq[Option[Map[String, String]]])
```

This fails, because while `Map[String, String]` _does_ have an `Eq` instance, it doesn't have an `Order` instance but our instance demands that `A: Order`. Classically the solution to this problem one would use Scala's lower priority implicit behavior to provide multiple instances, in this case we'd need to provide one for `PartialOrder` and `Eq`, having 3 different implicit scopes.

```scala mdoc:compile-only

object Option extends OptionLowPriorityInstances0 {
  def orderInstanceForOption[A: Order]: Order[Option[A]] =
    Order.from{
      case (Some(x), Some(y)) =>
        x.compare(y)
      case (None, None) =>
        0
      case (Some(_), _) =>
        1
      case _ =>
        -1
    }
}

trait OptionLowPriorityInstances0 extends OptionLowPriorityInstances1 {
  def partialOrderInstanceForOption[A: PartialOrder]: PartialOrder[Option[A]] =
    PartialOrder.from{
      case (Some(x), Some(y)) =>
        x.partialCompare(y)
      case (None, None) =>
        0d
      case (Some(_), _) =>
        1d
      case _ =>
        -1d
    }
}

trait OptionLowPriorityInstances1 {
  def eqInstanceForOption[A: Eq]: Eq[Option[A]] =
    Eq.instance{
      case (Some(x), Some(y)) =>
        x === y
      case (None, None) =>
        true
      case _ =>
        false
    }
}
```

However if we write an `Order1` instance, this is no longer the case.

```scala mdoc
implicit val order1InstanceForOption: Order1[Option] =
  new Order1[Option] {
    override def liftCompare[A, B](compare: (A, B) => Int, x: Option[A], y: Option[B]): Int =
      (x, y) match {
        case (Some(x), Some(y)) =>
          compare(x, y)
        case (None, None) =>
          0
        case (Some(_), _) =>
          1
        case _ =>
         -1
      }
  }

Order[Option[Int]].compare(Some(1), Some(2))

// Map doesn't have an Order/PartialOrder instance, but it does have an Eq Instance
Eq[Option[Map[String, String]]].eqv(Some(Map.empty), Some(Map.empty))
```

This works because `Order1` doesn't put any constraints on the concrete type until the moment we need to generate the `Order[Option[A]]` instance, which means that `Order1` can be used to generated `PartialOrder` values for types `A` which do not have an `Order` instance, and similarly for `Eq`.

In this way using these higher kinded classes can allow much more succinct definitions for canonical data types.

# Order2 and Friends

Similar to the `Order1`, `PartialOrder1`, and `Eq1`, there is also `Order2`, `PartialOrder2` and `Eq2`. These abstract over types with two type parameters, e.g. `Order2[F[_, _]]`. The semantics of these types are analogous to those of `Order1` and friends. In theory, one can define `OrderN` for arbitrarily number of type parameters, but in practice anything higher than two is rarely used.
