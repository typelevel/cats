---
layout: default
title:  "Functor"
section: "typeclasses"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/Functor.scala"
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

```scala
scala> Option(1).map(_ + 1)
res0: Option[Int] = Some(2)

scala> List(1,2,3).map(_ + 1)
res1: List[Int] = List(2, 3, 4)

scala> Vector(1,2,3).map(_.toString)
res2: scala.collection.immutable.Vector[String] = Vector(1, 2, 3)
```

## Creating Functor instances

We can trivially create a `Functor` instance for a type which has a well
behaved `map` method:

```scala
scala> import cats._
import cats._

scala> implicit val optionFunctor: Functor[Option] = new Functor[Option] {
     |   def map[A,B](fa: Option[A])(f: A => B) = fa map f
     | }
optionFunctor: cats.Functor[Option] = $anon$1@6c1dc0a

scala> implicit val listFunctor: Functor[List] = new Functor[List] {
     |   def map[A,B](fa: List[A])(f: A => B) = fa map f
     | }
listFunctor: cats.Functor[List] = $anon$1@6a443b00
```

However, functors can also be created for types which don't have a `map`
method. For example, if we create a `Functor` for `Function1[In, ?]`
we can use `andThen` to implement `map`:

```scala
scala> implicit def function1Functor[In]: Functor[Function1[In, ?]] =
     |   new Functor[Function1[In, ?]] {
     |     def map[A,B](fa: In => A)(f: A => B): Function1[In,B] = fa andThen f
     |   }
function1Functor: [In]=> cats.Functor[[β]In => β]
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

```scala
scala> val len: String => Int = _.length
len: String => Int = <function1>

scala> Functor[List].map(List("qwer", "adsfg"))(len)
res3: List[Int] = List(4, 5)
```

`Option` is a functor which only applies the function when the `Option` value 
is a `Some`:

```scala
scala> // Some(x) case: function is applied to x; result is wrapped in Some
     | Functor[Option].map(Some("adsf"))(len)
res5: Option[Int] = Some(4)

scala> // None case: simply returns None (function is not applied)
     | Functor[Option].map(None)(len)
res7: Option[Int] = None
```

## Derived methods

### lift

We can use `Functor` to "lift" a function from `A => B` to `F[A] => F[B]`:

```scala
scala> val lenOption: Option[String] => Option[Int] = Functor[Option].lift(len)
lenOption: Option[String] => Option[Int] = <function1>

scala> lenOption(Some("abcd"))
res8: Option[Int] = Some(4)
```

### fproduct

`Functor` provides an `fproduct` function which pairs a value with the
result of applying a function to that value.

```scala
scala> val source = List("a", "aa", "b", "ccccc")
source: List[String] = List(a, aa, b, ccccc)

scala> Functor[List].fproduct(source)(len).toMap
res9: scala.collection.immutable.Map[String,Int] = Map(a -> 1, aa -> 2, b -> 1, ccccc -> 5)
```

### compose

Functors compose! Given any functor `F[_]` and any functor `G[_]` we can
create a new functor `F[G[_]]` by composing them:

```scala
scala> val listOpt = Functor[List] compose Functor[Option]
listOpt: cats.Functor[[X]List[Option[X]]] = cats.Functor$$anon$1@1a1f722f

scala> listOpt.map(List(Some(1), None, Some(3)))(_ + 1)
res10: List[Option[Int]] = List(Some(2), None, Some(4))

scala> val optList = Functor[Option] compose Functor[List]
optList: cats.Functor[[X]Option[List[X]]] = cats.Functor$$anon$1@3011f9ae

scala> optList.map(Some(List(1, 2, 3)))(_ + 1)
res11: Option[List[Int]] = Some(List(2, 3, 4))

scala> val listOptList = listOpt compose Functor[List]
listOptList: cats.Functor[[X]List[Option[List[X]]]] = cats.Functor$$anon$1@1a810d64

scala> listOptList.map(List(Some(List(1,2)), None, Some(List(3,4))))(_ + 1)
res12: List[Option[List[Int]]] = List(Some(List(2, 3)), None, Some(List(4, 5)))
```
