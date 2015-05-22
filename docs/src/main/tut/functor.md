---
layout: default
title:  "Functors"
section: "typeclasses"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/Functor.scala"
scaladoc: "#cats.Functor"
---
# Functor

A Functor is a ubiquitous typeclass involving types that have "one
hole"; that is types which have the shape: `F[?]`, such as `Option`,
`List`, `Future`. (This is in contrast to a type like `Int` which has
no hole, or `Tuple2` which has two "holes" (`Tuple2[?,?]`), etc.

The Functor category involves a single operation, named `map`:

```scala
def map[A, B](fa: F[A])(f: A => B): F[B]
```

This method takes a function from A => B and turns an F[A] into an
F[B].  The name of the method `map` should remind you of the `map`
method that exists on many classes in the Scala standard library. Some
examples of map functions:

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

However, functors can also be created for types which don't have a map
method. An example of this would be that Functions which take a String
form a functor using andThen as the map operation:

```tut
implicit def function1Functor[In]: Functor[Function1[In, ?]] =
  new Functor[Function1[In, ?]] {
    def map[A,B](fa: In => A)(f: A => B): Function1[In,B] = fa andThen f
  }
```

Also of note in the above example, is that we created a functor for
Function1, which is a type which normally has two type holes. We
however constrained one of the holes to be the `In` type, leaving just
one hole for the return type. In this above example, we are
demonstrating the use of the
[kind-projector compiler plugin](https://github.com/non/kind-projector),
This compiler plugin lets us more easily change the number of type
holes a type has. In this case, we took a type which normally has two
type holes, `Function1` and filled one of the holes, leaving the other
hole open. `Function1[In,?]` has the first type parameter filled,
while the second is still open. Without kind-projector, we'd have to
write this as something like: `({type F[A] = Function1[In,A]})#F`,
which is much harder to read and understand.

## Using functor

### map

Option is a functor which always returns a Some with the function
applied when the Option value is a Some.

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
