# NonEmptyList

API Documentation: @:api(cats.data.NonEmptyList)

## Motivation

We start with two examples of `NonEmptyList`s

### Usage in `Validated` and `Ior`

If you have had the opportunity of taking a look to
[Validated](validated.md) or [Ior](ior.md), you'll find that a
common case is to use `NonEmptyList` with one of these data
structures.

Why? Because it fits nicely in the error reporting cases. As stated by
its name, `NonEmptyList` is a _specialized_ data type that has at
least *one* element.  Otherwise it behaves like a normal `List`.  For
sum types like `Validated` (and `Ior`), it does not make sense to have
a `Invalid` with no errors: no errors means it is a `Valid`!  By using
`NonEmptyList`, we explicitly say in the type that:

*If* it is a `Invalid`, *then* there is at least one error.

This is much more precise and we don't have to wonder whether the list
of errors might be empty when reporting them later on.

### Avoiding `Option` by demanding more specific arguments

As functional programmers, we naturally shy away from partial
functions that can throw exceptions like the famous `head` method on,
e.g., `List`.

Let's take as an example a method that calculates the `average`:

```scala mdoc:silent
def average(xs: List[Int]): Double = {
  xs.sum / xs.length.toDouble
}
```

Clearly, this is not a valid definition for *empty* lists, because
division by zero will throw an exception.  To fix this, one way is to
return an `Option` instead of a `Double` right away:

```scala mdoc:silent:nest
def average(xs: List[Int]): Option[Double] = if (xs.isEmpty) {
  None
} else {
  Some(xs.sum / xs.length.toDouble)
}
```

That works and is safe, but this only masks the problem of *accepting
invalid input*.  By using `Option`, we extend the `average` function
with the logic to handle empty lists.  Additionally, all callers have
to handle the `Option` cases, maybe over and over again.  While better
than failing with an exception, this is far from perfect.

Instead what we would like to express is that `average` does not make
sense at all for an empty list.  Luckily, cats defines the
`NonEmptyList`.  As the name says, this represents a list that
*cannot*, by construction, be empty.  So given a `NonEmptyList[A]` you
can be sure that there is at least one `A` in there.

Let's see how that impacts your `average` method:

```scala mdoc:silent:nest
import cats.data.NonEmptyList
def average(xs: NonEmptyList[Int]): Double = {
  xs.reduceLeft(_+_) / xs.length.toDouble
}
```

With that, `average` is free of any "domain invariant validation" and
instead can focus on the actual logic of computing the average of the
list.  This ties in nicely with the recommendation of shifting your
validation to the very borders of your program, where the input enters
your system.

## Structure of a NonEmptyList

`NonEmptyList` is defined as follows:

```scala
final case class NonEmptyList[+A](head: A, tail: List[A]) {
  // Implementation elided
}
```

The `head` of the `NonEmptyList` will be _non-empty_. Meanwhile, the
`tail` can have zero or more elements contained in a `List`.

## Defined for all its elements

An important trait of `NonEmptyList` is the totality. For `List` specifically,
both `head` and `tail` are partial: they are only well-defined if it
has at least one element.

`NonEmptyList` on the other hand, _guarantees_ you that operations like
`head` and `tail` are defined, because constructing an empty
`NonEmptyList` is simply not possible!

## Constructing a NonEmptyList

To construct a `NonEmptyList` you have different possibilities.

### Using `one`

If you want to construct a collection with only one argument, use
`NonEmptyList.one`:

```scala mdoc
NonEmptyList.one(42)
```

### Using `of`

The `NonEmptyList.of` method on the companion of `NonEmptyList` as the signature:

```scala
def of[A](head: A, tail: A*): NonEmptyList[A]
```

It accepts an argument list with at least one `A` followed by a
*varargs* argument for the `tail`.  Call it like this:

```scala mdoc
NonEmptyList.of(1)
NonEmptyList.of(1, 2)
NonEmptyList.of(1, 2, 3, 4)
```

There also is `ofInitLast` which takes a normal `List[A]` for the
prefix and a last element:

```scala mdoc
NonEmptyList.ofInitLast(List(), 4)
NonEmptyList.ofInitLast(List(1,2,3), 4)
```

### Using `fromList`

There is also `NonEmptyList.fromList` which returns an
`Option[NonEmptyList[A]]`:

```scala mdoc
NonEmptyList.fromList(List())
NonEmptyList.fromList(List(1,2,3))
```

Last but not least, there is `.toNel` if you import the syntax for
`list`:

```scala mdoc
import cats.syntax.list._
List(1,2,3).toNel
```

### Using `fromFoldable` and `fromReducible`

Even more general, you can use `NonEmptyList.fromFoldable` and
`NonEmptyList.fromReducible`.  The difference between the two is that
`fromReducible` can avoid the `Option` in the return type, because it
is only available for non-empty datastructures.

Here are some examples:

```scala mdoc
import cats.syntax.all._

NonEmptyList.fromFoldable(List())
NonEmptyList.fromFoldable(List(1,2,3))

NonEmptyList.fromFoldable(Vector(42))
NonEmptyList.fromFoldable(Vector(42))

// Everything that has a Foldable instance!
NonEmptyList.fromFoldable(Either.left[String, Int]("Error"))
NonEmptyList.fromFoldable(Either.right[String, Int](42))

// Avoid the Option for things with a `Reducible` instance
import cats.data.NonEmptyVector
NonEmptyList.fromReducible(NonEmptyVector.of(1, 2, 3))
```
