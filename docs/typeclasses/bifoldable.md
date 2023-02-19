# Bifoldable

API Documentation: @:api(cats.Bifoldable)

`Bifoldable[F[_,_]]` instances identify data structures with two independent `Foldable` that fold to the same summary value.

As a reminder `Foldable` is implemented in terms of `foldLeft` and `foldRight`; similarly `Bifoldable` is implemented in terms of:
```scala
  //eagerly performs a left-associative bi-fold over `fab`
  def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C

  //lazily performs a right-associative bi-fold over `fab`
  def bifoldRight[A, B, C](fab: F[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C]
```
and by implementing those 2 methods you also get:
```scala
  def bifold[A, B](fab: F[A, B])(implicit A: Monoid[A], B: Monoid[B]): (A, B)

  def bifoldMap[A, B, C](fab: F[A, B])(f: A => C, g: B => C)(implicit C: Monoid[C]): C
```
A lawful instance must have `bifoldLeft\Right` consistent with `bifoldMap`; left\right bi-folds based on associative
functions should output similar results.

## Either and Validated as Bifoldable

Assume multiple input paths and system requests that end up as `Either[Exception, *]` or `Validated[Exception, *]`.
The requirement is to track the amount of errors and store the content of valid requests.

First add the implicits:
```scala mdoc
import cats._
import cats.data._
import cats.syntax.all._
```

then let's define a summary class capable of storing this info:
```scala mdoc
case class Report(entries: Chain[String], errors: Int) {
  def withEntries(entry: String): Report =
    this.copy(entries = entries :+ entry)

  def withError: Report =
    this.copy(errors = errors + 1)
}
```

`Bifoldable` is useful to get a summary value from `F[_,_]` data types:
```scala mdoc
def update[F[_, _]: Bifoldable](current: Report)(result: F[Exception, String]): Report =
  result.bifoldLeft(current)((acc, _) => acc.withError, (acc, user) => acc.withEntries(user))
```

Here is the system input:
```scala mdoc
val validated =
  List(
    Validated.valid[Exception, String]("valid request 1"),
    Validated.valid[Exception, String]("valid request 2"),
    Validated.invalid[Exception, String](new RuntimeException("Not a valid request"))
  )

val attempted =
  List(
    Either.right[Exception, String]("valid request 1"),
    Either.right[Exception, String]("valid request 2"),
    Either.left[Exception, String](new RuntimeException("Not a valid request"))
  )
```

and bi-fold each value into the accumulator:
```scala mdoc
val empty = Report(Chain.empty, 0)

validated
  .foldl(empty)((acc, validation) => update(acc)(validation))

attempted
  .foldl(empty)((acc, attempt) => update(acc)(attempt))
```

## Tuple as Bifoldable

Assume we have `(String, String, Int)`  and to get our summary we need `_1` and `_3`.
The existing implementations for `(*, *)`, `(T0, *, *)`, `(T0, T1, *, *)` .. aren't useful.

Let's make a new `Bifoldable` instance:
```scala mdoc
implicit def bifoldableForTuple3[A0]: Bifoldable[(*, A0, *)] =
  new Bifoldable[(*, A0, *)] {
    def bifoldLeft[A, B, C](fa: (A, A0, B), c: C)(f: (C, A) => C, g: (C, B) => C): C =
      g(f(c, fa._1), fa._3)

    def bifoldRight[A, B, C](fa: (A, A0, B), c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
      g(fa._3, f(fa._1, c))
  }
```
As we were saying in the beginning a lawful `Bifoldable` should have `bifoldLeft\Right` consistent with `bifoldMap`.
Let's check our instance:
```scala mdoc
//(name, age, occupation)
val description = ("Niki", 22, "Developer")

val expected =
  Bifoldable[(*, Int, *)].bifoldMap(description)(s => s, s => s)

val left =
 Bifoldable[(*, Int, *)].bifoldLeft(description, Monoid[String].empty)(
  (acc, s) => acc |+| s,
  (acc, s) => acc |+| s
 )

val right =
  Bifoldable[(*, Int, *)].bifoldRight(description, Eval.later(Monoid[String].empty))(
    (s, acc) => acc.map(_ |+| s),
    (s, acc) => acc.map(_ |+| s)
  )

left === expected
right.value === expected
```
**NOTE:** This instance would not be lawful if `bifoldRight` in particular would use a different ordering.

Going from right to left as opposed to left to right means:
```scala mdoc
 def bifoldRight[A, B, C](fa: (A, Int, B), c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
   f(fa._1, g(fa._3, c))
```
and it would also reverse the output making the instance unlawful:
```scala mdoc
val reversedRight =
 bifoldRight(description, Eval.later(Monoid[String].empty))(
    (s, acc) => acc.map(_ |+| s),
    (s, acc) => acc.map(_ |+| s)
  )

reversedRight.value === expected
```

## Bifoldable `compose`

If `F[_,_]` and `G[_,_]` have `Bifoldable` instances then so does `F[G[_,_],G[_,_]]`.

Examples for `(Either[*, *], Either[*, *])` and `((*, *), (*, *))`:
```scala mdoc
Bifoldable[(*, *)]
  .compose(Bifoldable[(*, *)])
  .bifoldLeft((("name1 ", 1000),("name2 ", 2000)), List.empty[String])((acc, name) => acc :+ name, (acc, _) => acc)

Bifoldable[(*, *)]
  .compose(Bifoldable[Either[*, *]])
  .bifold((Left(1), Right(2)))
```

