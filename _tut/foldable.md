---
layout: default
title:  "Foldable"
section: "typeclasses"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/Foldable.scala"
scaladoc: "#cats.Foldable"
---
# Foldable

Foldable type class instances can be defined for data structures that can be 
folded to a summary value.

In the case of a collection (such as `List` or `Set`), these methods will fold 
together (combine) the values contained in the collection to produce a single 
result. Most collection types have `foldLeft` methods, which will usually be 
used by the associated `Foldable[_]` instance.

`Foldable[F]` is implemented in terms of two basic methods:

 - `foldLeft(fa, b)(f)` eagerly folds `fa` from left-to-right.
 - `foldRight(fa, b)(f)` lazily folds `fa` from right-to-left.
 
These form the basis for many other operations, see also: 
[A tutorial on the universality and expressiveness of fold](https://www.cs.nott.ac.uk/~gmh/fold.pdf) 

```scala
scala> import cats._
import cats._

scala> import cats.std.all._
import cats.std.all._

scala> Foldable[List].fold(List("a", "b", "c"))
res0: String = abc

scala> Foldable[List].foldMap(List(1, 2, 4))(_.toString)
res1: String = 124

scala> Foldable[List].foldK(List(List(1,2,3), List(2,3,4)))
res2: List[Int] = List(1, 2, 3, 2, 3, 4)

scala> Foldable[List].reduceLeftToOption(List[Int]())(_.toString)((s,i) => s + i)
res3: Option[String] = None

scala> Foldable[List].reduceLeftToOption(List(1,2,3,4))(_.toString)((s,i) => s + i)
res4: Option[String] = Some(1234)

scala> Foldable[List].reduceRightToOption(List(1,2,3,4))(_.toString)((i,s) => Later(s.value + i)).value
res5: Option[String] = Some(4321)

scala> Foldable[List].reduceRightToOption(List[Int]())(_.toString)((i,s) => Later(s.value + i)).value
res6: Option[String] = None

scala> Foldable[Set].find(Set(1,2,3))(_ > 2)
res7: Option[Int] = Some(3)

scala> Foldable[Set].exists(Set(1,2,3))(_ > 2)
res8: Boolean = true

scala> Foldable[Set].forall(Set(1,2,3))(_ > 2)
res9: Boolean = false

scala> Foldable[Set].forall(Set(1,2,3))(_ < 4)
res10: Boolean = true

scala> Foldable[Vector].filter_(Vector(1,2,3))(_ < 3)
res11: List[Int] = List(1, 2)

scala> Foldable[List].isEmpty(List(1,2))
res12: Boolean = false

scala> Foldable[Option].isEmpty(None)
res13: Boolean = true

scala> Foldable[List].nonEmpty(List(1,2))
res14: Boolean = true

scala> Foldable[Option].toList(Option(1))
res15: List[Int] = List(1)

scala> Foldable[Option].toList(None)
res16: List[Nothing] = List()

scala> def parseInt(s: String): Option[Int] = scala.util.Try(Integer.parseInt(s)).toOption
parseInt: (s: String)Option[Int]

scala> Foldable[List].traverse_(List("1", "2"))(parseInt)
res17: Option[Unit] = Some(())

scala> Foldable[List].traverse_(List("1", "A"))(parseInt)
res18: Option[Unit] = None

scala> Foldable[List].sequence_(List(Option(1), Option(2)))
res19: Option[Unit] = Some(())

scala> Foldable[List].sequence_(List(Option(1), None))
res20: Option[Unit] = None

scala> Foldable[List].dropWhile_(List[Int](2,4,5,6,7))(_ % 2 == 0)
res21: List[Int] = List(2, 4, 5, 6, 7)

scala> Foldable[List].dropWhile_(List[Int](1,2,4,5,6,7))(_ % 2 == 0)
res22: List[Int] = List(2, 4, 5, 6, 7)

scala> val FoldableListOption = Foldable[List].compose[Option]
FoldableListOption: cats.Foldable[[α]List[Option[α]]] = cats.Foldable$$anon$1@3a0b1aa5

scala> FoldableListOption.fold(List(Option(1), Option(2), Option(3), Option(4)))
res23: Int = 10

scala> FoldableListOption.fold(List(Option(1), Option(2), Option(3), None))
res24: Int = 6

scala> FoldableListOption.fold(List(Option("1"), Option("2"), Option("3"), None))
res25: String = 123
```

Hence when defining some new data structure, if we can define a `foldLeft` and
`foldRight` we are able to provide many other useful operations, if not always
 the most efficient implementations, over the structure without further 
 implementation.
 
-------------------------------------------------------------------------------
 
Note that, in order to support laziness, the signature of `Foldable`'s 
`foldRight` is 

```
def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
```

as opposed to
 
```
def foldRight[A, B](fa: F[A], z: B)(f: (A, B) => B): B
```
 
which someone familiar with the `foldRight` from the collections in Scala's standard
library might expect. 
