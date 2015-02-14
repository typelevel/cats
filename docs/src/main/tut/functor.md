---
layout: default
title:  "Functors"
section: "typeclasses"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/Functor.scala"
scaladoc: "#cats.Functor"
---
# Functor

A Functor is a ubiquitous typeclass involving type constructors of
kind * → *, which is another way of saying types that have a single
type variable. Examples might be Option, List, Future.

The Functor category involves a single operation, named `map`:

```scala
def map[A, B](fa: F[A])(f: A => B): F[B]
```

This method takes a Function from A => B and turns an F[A] into an
F[B].  The name of the method `map` should remind you of the `map`
method that exists on many classes in the scala standard library. some
Examples of map functions:

```tut
Option(1).map(_ + 1)
List(1,2,3).map(_ + 1)
Vector(1,2,3).map(_.toString)
```

## Creating Functor instances

We can trivially create a functor instance for a type which has a well
  behaved map method:

```tut
import cats._
implicit val optionFunctor: Functor[Option] = new Functor[Option] {
  def map[A,B](fa: Option[A])(f: A => B) = fa map f
}
implicit val listFunctor: Functor[List] = new Functor[List] {
  def map[A,B](fa: List[A])(f: A => B) = fa map f
}
```

However Functors can also be creted for types which don't have a map
method. An example of this would be that Functions which take a String
form a functor using andThen as the map operation:

```tut
implicit def function1Functor[In]: Functor[({type λ[α] = Function1[In,α]})#λ] =
  new Functor[({type λ[α] = Function1[In,α]})#λ] {
    def map[A,B](fa: In => A)(f: A => B): Function1[In,B] = fa andThen f
  }
```

## Using functor

### map

Option is a functor which always returns a Some with the function
applied when the Option value is a Some.
g
```tut
val len: String => Int = _.length
Functor[Option].map(Some("adsf"))(len)
// When the Option is a None, it always returns None
Functor[Option].map(None)(len)
```

List is a functor which applies the function to each element the list.
```tut
Functor[List].map(List("qwer", "adsfg"))(len)
```

## Derived methods

### lift

 We can use the Funtor to "lift" a function to operate on the Functor type:

```tut
val lenOption: Option[String] => Option[Int] = Functor[Option].lift(len)
lenOption(Some("abcd"))
```

### fproduct

Functor provides a fproduct function which pairs a value with the
result of applying a function to that value.

```tut
val source = List("a", "aa", "b", "ccccc")
Functor[List].fproduct(source)(len).toMap
```

## Composition

Functors compose! Given any Functor F[\_] and any Functor G[\_] we can
compose the two Functors to create a new Functor on F[G[\_]]:

```tut
val listOpt = Functor[List] compose Functor[Option]
listOpt.map(List(Some(1), None, Some(3)))(_ + 1)
```
