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

Let's examine an instance to get a feel for things. A useful instance of `Alternative` is that for `Vector`.

The relevant imports:

```tut:book:reset:silent
import cats.Alternative
import cats.implicits._ // should go away when I find all the implicits I need for as
```

And what we can do with them:

```tut:book
val empty = Alternative[Vector].empty[Int]
val pureOfFive = 5.pure [Vector]
val concatenated = 7.pure[Vector] <+> 8.pure[Vector]
val double: Int => Int = _ * 2
val addFive: Int => Int = _ + 5
val apForVectors = (double.pure[Vector] <+> addFive.pure[Vector]) ap concatenated
```

Of course, like other type classes, `Alternative` instances must obey some laws, in addition to those otherwise applying to `MonoidK` and `Applicative instances`:

* Right Absorption: Applying a contextual function `F[A => B]` to `empty [A]` should be `empty [B]`.
  * `ff ap F.empty[A] = F.empty[B]`.
* Left Distributivity:  Mapping over a combined element must be the combinations of the mapped elements.
  * `(fa <+> fa2) map f = ((fa map f) <+> (fa2 map f))` where `fa: F[A]` and `fb: F[B]` and `f: A => B`.
* Right Distributivity: Applying the combination of two functions must be the combination of their applications.
  * `(ff <+> fg) ap fa = (ff ap fa) <+> (fg ap fa)` where `ff: F[A => B]`, `fg: F[A => B]`, and `fa: F[A]`.

Morally, these laws guarantee the compatibility of the otherwise possibly independent `Applicative` and `MonoidK` structures.

## Making choices with `Alternative`

Perhaps surprisingly, combining these properties leads to interesting new capabilities, especially if the `F` in question also has a `Monad` available. In particular, Cats provides three operations new to `Alternative`. Starting with the simplest we have `guard`.

`guard` has the following signature:

```
def guard(condition: Boolean): F[Unit] =
  if (condition) pure(()) else empty
```

Note that we needed both `pure` from `Applicative` and `empty` from `MonoidK` in order to write `guard`. So it is non-trivial!

How would we use `guard`? Well, we can use it to conditionally produce contextual values, allowing us to make choices within the context.
That means we can write a relatively concise generic filter function without writing a single `if` directly ourselves:

```tut:book
import cats.Foldable
def collectWhere[F[_]: Alternative: Foldable, A](p: A => Boolean)(fa: F[A]): F[A] =
  fa.foldLeft(Alternative[F].empty[A])((good: F[A], y: A) => good <+> Alternative[F].guard(p(y)).as(y))
def collectEvens[F[_]: Alternative: Foldable](fa: F[Int]) = collectWhere[F, Int](_ % 2 == 0)(fa)
collectEvens[Vector](Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
collectEvens[Option](Option(1))
collectEvens[Option](Option(2))
collectEvens[List](List(4, 6, 5))
```

But, of course, we can write filter perfectly fine using `Foldable`, so this is a not much of a win. The weakness here was our insistence on `Boolean` as a target for our choices! What if we generalize it?

Well, in that case, `Alternative` also gives us a notion of partitioning, as long as we also have a `Monad` available. This is what `separate` does.

The trick here is that the inner type constructor (essentially the replacement for `Boolean` as the target of our "predicate") must have a `Bifoldable` available. A great example of a `Bifoldable` is `Either`, and another is `Tuple2`.

Let's imagine that we're trying to make a bunch of independent possibly failure prone calls with a bunch of different `Int` inputs (say it's the id of a resource), each returning `Either[Int, Int]` where a left of an integer is the code modeling the failure we're given (say it's the HTTP code returned by a remote API we're calling), while right of an integer is the output of the calculation.

We can use `separate` to pull apart the failures and successes zipped with the input, letting us log failures and proceed with successes intelligently.

```tut:book
// A very opinionated server to be sure... its use of moduli suggestive
// Note to self: Use Legendre symbol of first five moduli to determine HTTP code
def requestResource(a: Int): Either[(Int, Int), (Int, Int)] =
  if (a % 4 == 0) Left((a, 400)) else if (a % 3 == 0) Left((a, 500)) else Right((a, a % 7))

val partitionedResults = ((requestResource _).pure[Vector] ap Vector(5, 6, 7, 99, 1200, 8, 22)).separate

```

Alternatively (no pun intended), in a totally different version of the same idea, we can lean on the instance of `Bitraverse` for `Tuple2` to try and partition cleanly the outcomes of two different calculations that each depend pairwise on the same input (maybe we have a primary key and we want to select both the remote id of the resources with that key, and those that are foreign key linked to the input key).

```tut:book
// Surprising regularity in this politico-geographical data model!
def getRegionAndDistrict(pkey: Int): (Int, Vector[Int]) = (5 * pkey, Vector(double, addFive) ap pkey.pure[Vector])

val regionsWithDistricts = (getRegionAndDistrict _).pure[Vector] ap Vector(5, 6, 7, 97, 1200, 8, 25)
val regionIds = regionsWithDistricts.separate._1
val districtIds = regionsWithDistricts.separate._2.flatten
```

Now, we've captured well the idea of separating, but what about unifying? It turns out, perhaps unsurprisingly given the presence of `combineK` that we have a novel operation for that as well. ``

## Validating with Alternatives

```tut:book
```

## Instances
So, what has an `Alternative`?
Cats provides at least the following (and encourages you to find your own):
- `List`
- `Vector`
- `Stream`
- `Option`
- `Queue`
- `ZipStream`
- various derivatives based on bootstrapping. (Should examples be provided?)

Note the absence of the `NonEmptyList` and `NonEmptyVector`!
Exercise for the reader, why are they not here?

Hint: What makes `guard` awkward and hard to write?

## Further Reading
An unordered list of references on `Alternative`:
- [Typeclassopedia On Alternative](https://wiki.haskell.org/Typeclassopedia#Failure_and_choice:_Alternative.2C_MonadPlus.2C_ArrowPlus) should check on license before publishing
- [Incredibly Technically Deep Stack Overflow Answer on Alternatives](https://stackoverflow.com/questions/13080606/confused-by-the-meaning-of-the-alternative-type-class-and-its-relationship-to) Should reach out to author before publishing
- [Wikibooks Haskell Intro to Alternative](https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus) should check in on license before linking
- [Current Hackage Doc](https://hackage.haskell.org/package/monadplus-1.4.2/docs/Control-Applicative-Alternative.html) licensing?

## See Also:
- [`Applicative`](`applicative.html`)
- [`MonoidK`](`monoidk.html`)
- [`Foldable`](`foldable.html`)
- Bitraverse
