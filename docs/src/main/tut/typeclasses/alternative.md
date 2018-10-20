---
layout: docs
title: "Alternative"
section: "typeclasses"
source: "core/src/main/scala/cats/Alternative.scala"
scaladoc: "#cats.Alternative"
---
# Alternative
Alternative extends [`Applicative`](`applicative.html`) with a [`MonoidK`](`monoidk.html`).
Let's stub out all the operations just to remind ourselves what that gets us.

```tut:book:silent
import cats.{Applicative, MonoidK}

trait Alternative[F[_]] extends Applicative[F] with MonoidK[F] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  def pure[A](a: A): F[A]

  def empty[A]: F[A]

  def combineK[A](x: F[A], y: F[A]): F[A]
}
```

As you might recall, `pure` wraps values in the context and `ap` allows us to do calculations in the context, while `combineK` allows us to combine, for any given type `A`, any two contextual values `F[A]` and `empty` provides the identity element for the combine operation.

Of course, like other type classes, `Alternative` instances must obey some laws, in addition to those otherwise applying to `MonoidK` and `Applicative instances`:

* Right Absorption: Applying a contextual function `F[A => B]` to `empty [A]` should be `empty [B]`.
  * `ff ap F.empty[A] = F.empty[B]`.
* Left Distributivity:  Mapping over a combined element must be the combinations of the mapped elements.
  * `(fa <+> fa2) map f = ((fa map f) <+> (fa2 map f))` where `fa: F[A]` and `fb: F[B]` and `f: A => B`.
* Right Distributivity: Applying the combination of two functions must be the combination of their applications.
  * `(ff <+> fg) ap fa = (ff ap fa) <+> (fg ap fa)` where `ff: F[A => B]`, `fg: F[A => B]`, and `fa: F[A]`.

Morally, these laws guarantee the compatibility of the otherwise possibly independent `Applicative` and `MonoidK` structures.

## Vector as an Alternative

Let's examine an instance to get a feel for things. A useful instance of `Alternative` is that for `Vector`.

The relevant imports:

```tut:book:reset:silent
import cats.Alternative
import cats.implicits._
```

And what we can do with them:

```tut:book
val empty = Alternative[Vector].empty[Int]
val pureOfFive = 5.pure[Vector]
val concatenated = 7.pure[Vector] <+> 8.pure[Vector]
val double: Int => Int = _ * 2
val addFive: Int => Int = _ + 5
val apForVectors = (double.pure[Vector] <+> addFive.pure[Vector]) ap concatenated
```

## Making choices with `Alternative`

### Alternative Parsing

Suppose we have a simple parser library with an interface something like:

```tut:book
trait Decoder[A] {
  def decode(in: String): Either[Throwable, A]
}
object Decoder {
  def from[A](f: String => Either[Throwable, A]): Decoder[A] =
    new Decoder[A] {
      def decode(in: String) = f(in)
    }
}
```

Then, we can implement an `Alternative` instance for this type like so:

```tut:book
implicit val decoderAlternative = new Alternative[Decoder] {
  def pure[A](a: A) = Decoder.from(Function.const(Right(a)))

  def empty[A] = Decoder.from(Function.const(Left(new Error("No dice."))))

  def combineK[A](l: Decoder[A], r: Decoder[A]): Decoder[A] =
    new Decoder[A] {
      def decode(in: String) = l.decode(in).orElse(r.decode(in))
    }

  def ap[A, B](ff: Decoder[A => B])(fa: Decoder[A]): Decoder[B] =
    new Decoder[B] {
      def decode(in: String) = fa.decode(in) ap ff.decode(in)
    }
}
```

The addition of the `Alternative` methods allows us to prioritize multiple strategies, compensating for inconsistencies in the source data.

```tut:book
def parseInt(s: String) = Either.catchNonFatal(s.toInt)
def parseIntFirstChar(s: String) = Either.catchNonFatal(2 * Character.digit(s.charAt(0), 10))

// Try first parsing the whole, then just the first character.
val decoder = Decoder.from(parseInt _) <+> Decoder.from(parseIntFirstChar _)

decoder.decode("555")
decoder.decode("5a")
```

### Partitioning Results

`Alternative` gives us a notion of partitioning, as long as we also have a `Monad` available. This is what `separate` does.

The trick here is that the inner type constructor (essentially the replacement for `Boolean` as the target of our "predicate") must have a `Bifoldable` available. A great example of a `Bifoldable` is `Either`, and another is `Tuple2`.

Let's imagine that we're trying to make a bunch of independent possibly failure prone calls with a bunch of different `Int` inputs (say it's the id of a resource), each returning `Either[Int, Int]` where a left of an integer is the code modeling the failure we're given (say it's the HTTP code returned by a remote API we're calling), while right of an integer is the output of the calculation.

We can use `separate` to pull apart the failures and successes zipped with the input, letting us log failures and proceed with successes intelligently.

```tut:book
// Resource holder returns (Request, Status)
def requestResource(a: Int): Either[(Int, Int), (Int, Int)] = {
  if (a % 4 == 0) Left((a, 400))
  else if (a % 3 == 0) Left((a, 500))
  else Right((a, 200))
}

val partitionedResults = 
  ((requestResource _).pure[Vector] ap Vector(5, 6, 7, 99, 1200, 8, 22)).separate
```

Alternatively (no pun intended), in a totally different version of the same idea, we can lean on the instance of `Bitraverse` for `Tuple2` to try and partition cleanly the outcomes of two different calculations that each depend pairwise on the same input (maybe we have a primary key and we want to select both the remote id of the resources with that key, and those that are foreign key linked to the input key).

```tut:book
// Surprising regularity in this politico-geographical data model!
def getRegionAndDistrict(pkey: Int): (Int, Vector[Int]) = (5 * pkey, (double.pure[Vector] <+> addFive.pure[Vector]) ap pkey.pure[Vector])

val regionsWithDistricts = (getRegionAndDistrict _).pure[Vector] ap Vector(5, 6, 7, 97, 1200, 8, 25)
val regionIds = regionsWithDistricts.separate._1
val districtIds = regionsWithDistricts.separate._2.flatten
```


