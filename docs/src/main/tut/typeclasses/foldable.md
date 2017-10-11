---
layout: docs
title:  "Foldable"
section: "typeclasses"
source: "core/src/main/scala/cats/Foldable.scala"
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
[A tutorial on the universality and expressiveness of fold](http://www.cs.nott.ac.uk/~gmh/fold.pdf)

First some standard imports.

```tut:silent
import cats._
import cats.implicits._
```

And examples.

```tut:book
Foldable[List].fold(List("a", "b", "c"))
Foldable[List].foldMap(List(1, 2, 4))(_.toString)
Foldable[List].foldK(List(List(1,2,3), List(2,3,4)))
Foldable[List].reduceLeftToOption(List[Int]())(_.toString)((s,i) => s + i)
Foldable[List].reduceLeftToOption(List(1,2,3,4))(_.toString)((s,i) => s + i)
Foldable[List].reduceRightToOption(List(1,2,3,4))(_.toString)((i,s) => Later(s.value + i)).value
Foldable[List].reduceRightToOption(List[Int]())(_.toString)((i,s) => Later(s.value + i)).value
Foldable[Set].find(Set(1,2,3))(_ > 2)
Foldable[Set].exists(Set(1,2,3))(_ > 2)
Foldable[Set].forall(Set(1,2,3))(_ > 2)
Foldable[Set].forall(Set(1,2,3))(_ < 4)
Foldable[Vector].filter_(Vector(1,2,3))(_ < 3)
Foldable[List].isEmpty(List(1,2))
Foldable[Option].isEmpty(None)
Foldable[List].nonEmpty(List(1,2))
Foldable[Option].toList(Option(1))
Foldable[Option].toList(None)

def parseInt(s: String): Option[Int] = scala.util.Try(Integer.parseInt(s)).toOption

Foldable[List].traverse_(List("1", "2"))(parseInt)
Foldable[List].traverse_(List("1", "A"))(parseInt)
Foldable[List].sequence_(List(Option(1), Option(2)))
Foldable[List].sequence_(List(Option(1), None))

Foldable[List].forallM(List(1, 2, 3))(i => if (i < 2) Some(i % 2 == 0) else None)
Foldable[List].existsM(List(1, 2, 3))(i => if (i < 2) Some(i % 2 == 0) else None)
Foldable[List].existsM(List(1, 2, 3))(i => if (i < 3) Some(i % 2 == 0) else None)

val prints: Eval[Unit] = List(Eval.always(println(1)), Eval.always(println(2))).sequence_
prints.value

Foldable[List].dropWhile_(List[Int](2,4,5,6,7))(_ % 2 == 0)
Foldable[List].dropWhile_(List[Int](1,2,4,5,6,7))(_ % 2 == 0)

import cats.data.Nested
val listOption0 = Nested(List(Option(1), Option(2), Option(3)))
val listOption1 = Nested(List(Option(1), Option(2), None))
Foldable[Nested[List, Option, ?]].fold(listOption0)
Foldable[Nested[List, Option, ?]].fold(listOption1)
```

Hence when defining some new data structure, if we can define a `foldLeft` and
`foldRight` we are able to provide many other useful operations, if not always
 the most efficient implementations, over the structure without further 
 implementation.
 
-------------------------------------------------------------------------------
 
Note that, in order to support laziness, the signature of `Foldable`'s 
`foldRight` is 

```scala
def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
```

as opposed to
 
```scala
def foldRight[A, B](fa: F[A], z: B)(f: (A, B) => B): B
```
 
which someone familiar with the `foldRight` from the collections in
Scala's standard library might expect. This will prevent operations
which are lazy in their right hand argument to traverse the entire
structure unnecessarily. For example, if you have:

```tut:book
val allFalse = Stream.continually(false)
```

which is an infinite stream of `false` values, and if you wanted to
reduce this to a single false value using the logical and (`&&`). You
intuitively know that the result of this operation should be
`false`. It is not necessary to consider the entire stream in order to
determine this result, you only need to consider the first
value. Using `foldRight` from the standard library *will* try to
consider the entire stream, and thus will eventually cause a stack
overflow:

```tut:book
try {
  allFalse.foldRight(true)(_ && _)
} catch {
  case e:StackOverflowError => println(e)
}
```

With the lazy `foldRight` on `Foldable`, the calculation terminates
after looking at only one value:

```tut:book
Foldable[Stream].foldRight(allFalse, Eval.True)((a,b) => if (a) b else Eval.now(false)).value
```
