# Monoid

`Monoid` extends the power of `Semigroup` by providing an additional `empty` value.

```scala mdoc:silent
trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}
```

This `empty` value should be an identity for the `combine` operation, which means the following equalities hold
for any choice of `x`.

```
combine(x, empty) = combine(empty, x) = x
```

Many types that form a `Semigroup` also form a `Monoid`, such as `Int`s (with `0`) and `Strings` (with `""`).

```scala mdoc:reset:silent
import cats.Monoid

implicit val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
  def empty: Int = 0
  def combine(x: Int, y: Int): Int = x + y
}

val x = 1
```

```scala mdoc
Monoid[Int].combine(x, Monoid[Int].empty)

Monoid[Int].combine(Monoid[Int].empty, x)
```

# Example usage: Collapsing a list

In the `Semigroup` section we had trouble writing a generic `combineAll` function because we had nothing
to give if the list was empty. With `Monoid` we can return `empty`, giving us

```scala mdoc:silent
def combineAll[A: Monoid](as: List[A]): A =
  as.foldLeft(Monoid[A].empty)(Monoid[A].combine)
```

which can be used for any type that has a `Monoid` instance.

```scala mdoc:silent
import cats.implicits._
```

```scala mdoc
combineAll(List(1, 2, 3))

combineAll(List("hello", " ", "world"))

combineAll(List(Map('a' -> 1), Map('a' -> 2, 'b' -> 3), Map('b' -> 4, 'c' -> 5)))

combineAll(List(Set(1, 2), Set(2, 3, 4, 5)))
```

This function is provided in Cats as `Monoid.combineAll`.

# The `Option` monoid

There are some types that can form a `Semigroup` but not a `Monoid`. For example, the
following `NonEmptyList` type forms a semigroup through `++`, but has no corresponding
identity element to form a monoid.

```scala mdoc:silent
import cats.Semigroup

final case class NonEmptyList[A](head: A, tail: List[A]) {
  def ++(other: NonEmptyList[A]): NonEmptyList[A] = NonEmptyList(head, tail ++ other.toList)

  def toList: List[A] = head :: tail
}

object NonEmptyList {
  implicit def nonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] = _ ++ _
}
```

How then can we collapse a `List[NonEmptyList[A]]` ? For such types that only have a `Semigroup` we can
lift into `Option` to get a `Monoid`.

```scala mdoc:silent
import cats.implicits._

implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
  def empty: Option[A] = None

  def combine(x: Option[A], y: Option[A]): Option[A] =
    x match {
      case None => y
      case Some(xv) =>
        y match {
          case None => x
          case Some(yv) => Some(xv |+| yv)
        }
    }
}
```

This is the `Monoid` for `Option`: for any `Semigroup[A]`, there is a `Monoid[Option[A]]`.

Thus:

```scala mdoc:reset:silent
import cats.Monoid
import cats.data.NonEmptyList
import cats.implicits._

val list = List(NonEmptyList(1, List(2, 3)), NonEmptyList(4, List(5, 6)))
val lifted = list.map(nel => Option(nel))
```

```scala mdoc
Monoid.combineAll(lifted)
```

This lifting and combining of `Semigroup`s into `Option` is provided by Cats as `Semigroup.combineAllOption`.

-----

N.B.
Cats defines  the `Monoid` type class in cats-kernel. The
[`cats` package object](https://github.com/typelevel/cats/blob/main/core/src/main/scala/cats/package.scala)
defines type aliases to the `Monoid` from cats-kernel, so that you can simply import `cats.Monoid`.
