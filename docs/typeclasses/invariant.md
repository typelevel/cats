# Invariant

API Documentation: @:api(cats.Invariant)

The `Invariant` type class is for functors that define an `imap`
function with the following type:

```scala
def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
```

Every covariant (as well as [contravariant](contravariant.md)) functor gives rise to an invariant
functor, by ignoring the `g` (or in case of contravariance, `f`) function.

Examples for instances of `Invariant` are [`Semigroup`](semigroup.md) and [`Monoid`](monoid.md), in
the following we will explain why this is the case using `Semigroup`, the
reasoning for `Monoid` is analogous.

## Invariant instance for Semigroup

Pretend that we have a `Semigroup[Long]` representing a standard UNIX
timestamp.  Let's say that we want to create a `Semigroup[Date]`, by
*reusing* `Semigroup[Long]`.

### Semigroup does not form a covariant functor

If `Semigroup` had an instance for the standard covariant [`Functor`](functor.md)
type class, we could use `map` to apply a function `longToDate`:

```scala mdoc:silent
import java.util.Date
def longToDate: Long => Date = new Date(_)
```

But is this enough to give us a `Semigroup[Date]`?  The answer is no,
unfortunately.  A `Semigroup[Date]` should be able to combine two
values of type `Date`, given a `Semigroup` that only knows how to
combine `Long`s!  The `longToDate` function does not help at all,
because it only allows us to convert a `Long` into a `Date`.  Seems
like we can't have an `Functor` instance for `Semigroup`.

### Semigroup does not form a contravariant functor

On the other side, if `Semigroup` would form a *contravariant* functor
by having an instance for [`Contravariant`](contravariant.md), we could make use of
`contramap` to apply a function `dateToLong`:

```scala mdoc:silent
import java.util.Date
def dateToLong: Date => Long = _.getTime
```

Again we are faced with a problem when trying to get a
`Semigroup[Date]` based on a `Semigroup[Long]`.  As before consider
the case where we have two values of `Date` at hand.  Using
`dateToLong` we can turn them into `Long`s and use `Semigroup[Long]`
to combine the two values.  We are left with a value of type `Long`,
but we can't turn it back into a `Date` using only `contramap`!

### Semigroup does form an invariant functor

From the previous discussion we conclude that we need both the `map`
from (covariant) `Functor` and `contramap` from `Contravariant`.
There already is a type class for this and it is called `Invariant`.
Instances of the `Invariant` type class provide the `imap` function:

```scala
def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
```

Reusing the example of turning `Semigroup[Long]` into
`Semigroup[Date]`, we can use the `g` parameter to turn `Date` into a
`Long`, combine our two values using `Semigroup[Long]` and then
convert the result back into a `Date` using the `f` parameter of
`imap`:

```scala mdoc:silent:reset
import java.util.Date

// import everything for simplicity:
import cats._
import cats.syntax.all._

def longToDate: Long => Date = new Date(_)
def dateToLong: Date => Long = _.getTime

implicit val semigroupDate: Semigroup[Date] =
  Semigroup[Long].imap(longToDate)(dateToLong)

val today: Date = longToDate(1449088684104l)
val timeLeft: Date = longToDate(1900918893l)
```

```scala mdoc
today |+| timeLeft
```
