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

```tut
import cats._
import cats.std.all._

Foldable[List].fold(List("a", "b", "c"))
Foldable[List].foldMap(List(1, 2, 4))(_.toString)
Foldable[Set].find(Set(1,2,3))(_ > 2)
Foldable[Set].exists(Set(1,2,3))(_ > 2)
Foldable[Set].forall(Set(1,2,3))(_ > 2)
Foldable[Set].forall(Set(1,2,3))(_ < 4)
Foldable[Vector].filter_(Vector(1,2,3))(_ < 3)
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
