# Parallel

API Documentation: @:api(cats.Parallel)

When browsing the various `Monads` included in Cats,
you may have noticed that some of them have data types that are actually of the same structure,
but instead have instances of `Applicative`. E.g. `Either` and `Validated`.

This is because defining a `Monad` instance for data types like `Validated` [would be inconsistent](../datatypes/validated.md#of-flatmaps-and-eithers) with its error-accumulating behaviour.
In short, `Monads` describe dependent computations and `Applicatives` describe independent computations.

Sometimes however, we want to use both in conjunction with each other.
In the example of `Either` and `Validated` it is trivial albeit cumbersome to convert between the two types.
Below is a short example of a situation where we might run into this.
For simplicity, we'll use `String` as our type to represent errors.

```scala mdoc
import cats.syntax.all._
import cats.data._

case class Name(value: String)
case class Age(value: Int)
case class Person(name: Name, age: Age)

def parse(s: String): Either[NonEmptyList[String], Int] = {
  if (s.matches("-?[0-9]+")) Right(s.toInt)
  else Left(NonEmptyList.one(s"$s is not a valid integer."))
}

def validateAge(a: Int): Either[NonEmptyList[String], Age] = {
  if (a > 18) Right(Age(a))
  else Left(NonEmptyList.one(s"$a is not old enough"))
}

def validateName(n: String): Either[NonEmptyList[String], Name] = {
  if (n.length >= 8) Right(Name(n))
  else Left(NonEmptyList.one(s"$n Does not have enough characters"))
}

```

Now we want to parse two Strings into a value of `Person`:

```scala mdoc
def parsePerson(ageString: String, nameString: String) =
  for {
    age <- parse(ageString)
    person <- (validateName(nameString).toValidated, validateAge(age).toValidated)
       .mapN(Person)
       .toEither
  } yield person

```

We had to convert to and from `Validated` manually.
While this is still manageble, it gets worse the more `Eithers` we want to combine in parallel.

To mitigate this pain, Cats introduces a type class `Parallel` that abstracts over `Monads` which also support parallel composition.
It is simply defined in terms of conversion functions between the two data types:

```scala
trait Parallel[M[_]] {
  type F[_]

  def sequential: F ~> M

  def parallel: M ~> F
}
```
Where `M[_]` has to have an instance of `Monad` and `F[_]` an instance of `Applicative`.

![The `Parallel` typeclass transforms between `Monad` `M[_]` and `Applicative` `F[_]`.](../img/parallel.png)

Recall that `~>` is just an alias for [`FunctionK`](../datatypes/functionk.md).
This allows us to get rid of most of our boilerplate from earlier:

```scala mdoc:nest
def parsePerson(ageString: String, nameString: String) =
  for {
    age <- parse(ageString)
    person <- (validateName(nameString), validateAge(age)).parMapN(Person)
  } yield person
```

We can also traverse over a `Traverse` using `Parallel`:

```scala mdoc
List(Either.right(42), Either.left(NonEmptyList.one("Error 1")), Either.left(NonEmptyList.one("Error 2"))).parSequence
```



Parallel is also really useful for `zipping` collections. The standard `Applicative` instance for `List`, `Vector`, etc.
behaves like the cartesian product of the individual collections:

```scala mdoc
(List(1, 2, 3), List(4, 5, 6)).mapN(_ + _)
```

However often we will want to `zip` two or more collections together.
We can define a different `ap` for most of them and use the `parMapN` syntax for that:

```scala mdoc
(List(1, 2, 3), List(4, 5, 6)).parMapN(_ + _)
```

## NonEmptyParallel - a weakened Parallel

Some types cannot form a `Monad` or an `Applicative` because it's not possible to implement the `pure` function for them.
However, these types can often have instances for `FlatMap` or `Apply`.
For types like these we can use the `NonEmptyParallel` type class.
An example for one of these is `ZipList`.


With instances of `NonEmptyParallel` it's not possible to use the `parTraverse` and `parSequence` functions,
but we can still use `parMapN` and also `parNonEmptyTraverse` and `parNonEmptySequence`,
which are analogous to the functions defined on [`NonEmptyTraverse`](nonemptytraverse.md).
