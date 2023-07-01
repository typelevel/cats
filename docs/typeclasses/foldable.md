# Foldable

API Documentation: @:api(cats.Foldable)

Foldable type class instances can be defined for data structures that can be 
folded to a summary value.

In the case of a collection (such as `List` or `Vector`), these methods will fold
together (combine) the values contained in the collection to produce a single 
result. Most collection types have `foldLeft` methods, which will usually be 
used by the associated `Foldable[_]` instance.

`Foldable[F]` is implemented in terms of two basic methods:

 - `foldLeft(fa, b)(f)` eagerly performs a left-associative fold over `fa`.
 - `foldRight(fa, b)(f)` lazily performs a right-associative fold over `fa`.

Consider a simple list like `List(1, 2, 3)`. You could sum the numbers of this list using folds
where `0` is the starting value (`b`) and integer addition (`+`) is the combination operation
(`f`). Since `foldLeft` is left-associative, the execution of this fold would look something like
`((0 + 1) + 2) + 3`. The execution of a similar `foldRight`-based solution would look something
like `0 + (1 + (2 + 3))`. In this case, since integer addition is associative, both approaches will
yield the same result. However, for non-associative operations, the two methods can produce
different results.
 
These form the basis for many other operations, see also: 
[A tutorial on the universality and expressiveness of fold](http://www.cs.nott.ac.uk/~gmh/fold.pdf)

First some standard imports.

```scala mdoc:silent
import cats._
import cats.syntax.all._
```

And examples.

```scala mdoc
Foldable[List].fold(List("a", "b", "c"))
Foldable[List].foldMap(List(1, 2, 4))(_.toString)
Foldable[List].foldK(List(List(1,2,3), List(2,3,4)))
Foldable[List].reduceLeftToOption(List[Int]())(_.toString)((s,i) => s + i)
Foldable[List].reduceLeftToOption(List(1,2,3,4))(_.toString)((s,i) => s + i)
Foldable[List].reduceRightToOption(List(1,2,3,4))(_.toString)((i,s) => Later(s.value + i)).value
Foldable[List].reduceRightToOption(List[Int]())(_.toString)((i,s) => Later(s.value + i)).value
Foldable[List].find(List(1,2,3))(_ > 2)
Foldable[List].exists(List(1,2,3))(_ > 2)
Foldable[List].forall(List(1,2,3))(_ > 2)
Foldable[List].forall(List(1,2,3))(_ < 4)
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
Foldable[Nested[List, Option, *]].fold(listOption0)
Foldable[Nested[List, Option, *]].fold(listOption1)
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

```scala mdoc
val allFalse = LazyList.continually(false)
```

which is an infinite list of `false` values, and if you wanted to
reduce this to a single false value using the logical and (`&&`). You
intuitively know that the result of this operation should be
`false`. It is not necessary to consider the entire list in order to
determine this result, you only need to consider the first
value. Using `foldRight` from the standard library *will* try to
consider the entire list, and thus will eventually cause an out-of-memory error:

```scala
// beware! throws OutOfMemoryError, which is irrecoverable
allFalse.foldRight(true)(_ && _)
```

With the lazy `foldRight` on `Foldable`, the calculation terminates
after looking at only one value:

```scala mdoc
Foldable[LazyList].foldRight(allFalse, Eval.True)((a,b) => if (a) b else Eval.False).value
```

Unfortunately, since `foldRight` is defined on many collections - this
extension clashes with the operation defined in `Foldable`.
To get past this and make sure you're getting the lazy `foldRight` defined
in `Foldable`, there's an alias `foldr`:

```scala mdoc
allFalse.foldr(Eval.True)((a,b) => if (a) b else Eval.False).value
```
