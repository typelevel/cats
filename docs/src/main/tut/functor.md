---
layout: default
title:  "Functor"
section: "typeclasses"
source: "core/src/main/scala/cats/Functor.scala"
scaladoc: "#cats.Functor"
---
# Functor

A `Functor` is a ubiquitous type class involving types that have one
"hole", i.e. types which have the shape `F[?]`, such as `Option`,
`List` and `Future`. (This is in contrast to a type like `Int` which has
no hole, or `Tuple2` which has two holes (`Tuple2[?,?]`)).

The `Functor` category involves a single operation, named `map`:

```scala
def map[A, B](fa: F[A])(f: A => B): F[B]
```

This method takes a function `A => B` and turns an `F[A]` into an
`F[B]`.  The name of the method `map` should remind you of the `map`
method that exists on many classes in the Scala standard library, for
example:

```tut:book
Option(1).map(_ + 1)
List(1,2,3).map(_ + 1)
Vector(1,2,3).map(_.toString)
```

## Creating Functor instances

We can trivially create a `Functor` instance for a type which has a well
behaved `map` method:

```tut:silent
import cats._

implicit val optionFunctor: Functor[Option] = new Functor[Option] {
  def map[A,B](fa: Option[A])(f: A => B) = fa map f
}

implicit val listFunctor: Functor[List] = new Functor[List] {
  def map[A,B](fa: List[A])(f: A => B) = fa map f
}
```

However, functors can also be created for types which don't have a `map`
method. For example, if we create a `Functor` for `Function1[In, ?]`
we can use `andThen` to implement `map`:

```tut:silent
implicit def function1Functor[In]: Functor[Function1[In, ?]] =
  new Functor[Function1[In, ?]] {
    def map[A,B](fa: In => A)(f: A => B): Function1[In,B] = fa andThen f
  }
```

This example demonstrates the use of the
[kind-projector compiler plugin](https://github.com/non/kind-projector).
This compiler plugin can help us when we need to change the number of type
holes. In the example above, we took a type which normally has two type holes, 
`Function1[?,?]` and constrained one of the holes to be the `In` type, 
leaving just one hole for the return type, resulting in `Function1[In,?]`. 
Without kind-projector, we'd have to write this as something like 
`({type F[A] = Function1[In,A]})#F`, which is much harder to read and understand.

## Using Functor

### map

`List` is a functor which applies the function to each element of the list:

```tut:book
val len: String => Int = _.length
Functor[List].map(List("qwer", "adsfg"))(len)
```

`Option` is a functor which only applies the function when the `Option` value 
is a `Some`:

```tut:book
Functor[Option].map(Some("adsf"))(len) // Some(x) case: function is applied to x; result is wrapped in Some
Functor[Option].map(None)(len) // None case: simply returns None (function is not applied)
```

## Derived methods

### lift

We can use `Functor` to "lift" a function from `A => B` to `F[A] => F[B]`:

```tut:book
val lenOption: Option[String] => Option[Int] = Functor[Option].lift(len)
lenOption(Some("abcd"))
```

### fproduct

`Functor` provides an `fproduct` function which pairs a value with the
result of applying a function to that value.

```tut:book
val source = List("a", "aa", "b", "ccccc")
Functor[List].fproduct(source)(len).toMap
```

### compose

Functors compose! Given any functor `F[_]` and any functor `G[_]` we can
create a new functor `F[G[_]]` by composing them via the `Nested` data type:

```tut:book
import cats.data.Nested
val listOpt = Nested[List, Option, Int](List(Some(1), None, Some(3)))
Functor[Nested[List, Option, ?]].map(listOpt)(_ + 1)

val optList = Nested[Option, List, Int](Some(List(1, 2, 3)))
Functor[Nested[Option, List, ?]].map(optList)(_ + 1)
```

## Subtyping

Functors have a natural relationship with subtyping:

```tut:book
class A
class B extends A
val b: B = new B
val a: A = b
val listB: List[B] = List(new B)
val listA1: List[A] = listB.map(b => b: A)
val listA2: List[A] = listB.map(identity[A])
val listA3: List[A] = Functor[List].widen[B, A](listB)
```

Subtyping relationships are "lifted" by functors, such that if `F` is a
lawful functor and `A <: B` then `F[A] <: F[B]` - almost. Almost, because to
convert an `F[B]` to an `F[A]` a call to `map(identity[A])` is needed
(provided as `widen` for convenience). The functor laws guarantee that
`fa map identity == fa`, however.
