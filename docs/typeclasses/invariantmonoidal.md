# Invariant Monoidal

API Documentation: @:api(cats.InvariantMonoidal)

`InvariantMonoidal` combines [`Invariant`](invariant.md) and `Semigroupal` with the addition of a `unit` methods, defined in isolation the `InvariantMonoidal` type class could be defined as follows:

```scala mdoc:compile-only
trait InvariantMonoidal[F[_]] {
  def unit: F[Unit]
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}
```

Practical uses of `InvariantMonoidal` appear in the context of codecs, that is interfaces to capture both serialization and deserialization for a given format. Another notable examples is [`Semigroup`](semigroup.md).

This tutorial first shows how `Semigroup` is `InvariantMonoidal`, and how this can be used create `Semigroup` instances by combining other `Semigroup` instances. Secondly, we present a complete example of `Codec` for the CSV format, and show how it is `InvariantMonoidal`. Lastly, we present an alternative definition of `InvariantMonoidal` as a generalization of `Invariant`, and show that both definitions are equivalent.

# `Semigroup` is `InvariantMonoidal`

As explained in the [`Invariant` tutorial](invariant.md), `Semigroup` forms an invariant functor. Indeed, given a `Semigroup[A]` and two functions `A => B` and `B => A`, one can construct a `Semigroup[B]` by transforming two values from type `B` to type `A`, combining these using the `Semigroup[A]`, and transforming the result back to type `B`. Thus to define an `InvariantMonoidal[Semigroup]` we need implementations for `unit` and `product`.

To construct a `Semigroup` from a single value, we can define a trivial `Semigroup` with a combine that always outputs the given value. A `Semigroup[(A, B)]` can be obtained from two `Semigroup`s for type `A` and `B` by deconstructing two pairs into elements of type `A` and `B`, combining these element using their respective `Semigroup`s, and reconstructing a pair from the results:

```scala mdoc:silent
import cats.Semigroup

def unit: Semigroup[Unit] = (_, _) => ()

def product[A, B](fa: Semigroup[A], fb: Semigroup[B]): Semigroup[(A, B)] = {
  case ((xa, xb), (ya, yb)) => fa.combine(xa, ya) -> fb.combine(xb, yb)
}
```

Given an instance of `InvariantMonoidal` for `Semigroup`, we are able to combine existing `Semigroup` instances to form a new `Semigroup` by using the `Semigroupal` syntax:

```scala mdoc:silent
import cats.syntax.all._

// Let's build a Semigroup for this case class
case class Foo(a: String, c: List[Double])

implicit val fooSemigroup: Semigroup[Foo] = (
  (implicitly[Semigroup[String]], implicitly[Semigroup[List[Double]]])
    .imapN(Foo.apply)(Function.unlift(Foo.unapply))
)
```

Our new Semigroup in action:

```scala mdoc
Foo("Hello", List(0.0)) |+| Foo("World", Nil) |+| Foo("!", List(1.1, 2.2))
```

# `CsvCodec` is `InvariantMonoidal`

We define `CsvCodec`, a type class for serialization and deserialization of CSV rows:


```scala mdoc:silent
type CSV = List[String]

trait CsvCodec[A] {
  def read(s: CSV): (Option[A], CSV)
  def write(a: A): CSV
}
```

The `read` method consumes columns from a CSV row and returns an optional value and the remaining CSV. The `write` method produces the CSV representation of a given value.

Beside the composition capabilities illustrated later in this tutorial, grouping both serialization and deserialization in a single type class has the advantage to allows the definition of a law to capture the fact that both operations play nicely together:

```scala
forAll { (c: CsvCodec[A], a: A) => c.read(c.write(a)) == ((Some(a), List()))
```

Let's now see how we could define an `InvariantMonoidal` instance for `CsvCodec`. Lifting a single value into a `CsvCodec` can be done "the trivial way" by consuming nothing from CSV and producing that value, and writing this value as the empty CSV:

```scala mdoc:silent
trait CCUnit {
  def unit: CsvCodec[Unit] = new CsvCodec[Unit] {
    def read(s: CSV): (Option[Unit], CSV) = (Some(()), s)
    def write(u: Unit): CSV = List.empty
  }
}
```

Combining two `CsvCodec`s could be done by reading and writing each value of a pair sequentially, where reading succeeds if both read operations succeed:

```scala mdoc:silent
trait CCProduct {
  def product[A, B](fa: CsvCodec[A], fb: CsvCodec[B]): CsvCodec[(A, B)] =
    new CsvCodec[(A, B)] {
      def read(s: CSV): (Option[(A, B)], CSV) = {
        val (a1, s1) = fa.read(s)
        val (a2, s2) = fb.read(s1)
        ((a1, a2).mapN(_ -> _), s2)
      }

      def write(a: (A, B)): CSV =
        fa.write(a._1) ++ fb.write(a._2)
    }
}
```

Changing a `CsvCodec[A]` to `CsvCodec[B]` requires two functions of type `A => B` and `B => A` to transform a value from `A` to `B` after  deserialized, and from `B` to `A` before serialization:

```scala mdoc:silent
trait CCImap {
  def imap[A, B](fa: CsvCodec[A])(f: A => B)(g: B => A): CsvCodec[B] =
    new CsvCodec[B] {
      def read(s: CSV): (Option[B], CSV) = {
        val (a1, s1) = fa.read(s)
        (a1.map(f), s1)
      }

      def write(a: B): CSV =
        fa.write(g(a))
    }
}
```

Putting it all together:

```scala mdoc:silent
import cats.InvariantMonoidal

implicit val csvCodecIsInvariantMonoidal: InvariantMonoidal[CsvCodec] =
  new InvariantMonoidal[CsvCodec] with CCUnit with CCProduct with CCImap
```

We can now define a few `CsvCodec` instances and use the methods provided by `InvariantMonoidal` to define `CsvCodec` from existing `CsvCodec`s:

```scala mdoc:silent
val stringCodec: CsvCodec[String] =
  new CsvCodec[String] {
    def read(s: CSV): (Option[String], CSV) = (s.headOption, s.drop(1))
    def write(a: String): CSV = List(a)
  }

def numericSystemCodec(base: Int): CsvCodec[Int] =
  new CsvCodec[Int] {
    def read(s: CSV): (Option[Int], CSV) =
      (s.headOption.flatMap(head => scala.util.Try(Integer.parseInt(head, base)).toOption), s.drop(1))

    def write(a: Int): CSV =
      List(Integer.toString(a, base))
  }
```

```scala mdoc:silent:nest
case class BinDec(binary: Int, decimal: Int)

val binDecCodec: CsvCodec[BinDec] = (
  (numericSystemCodec(2), numericSystemCodec(10))
    .imapN(BinDec.apply)(Function.unlift(BinDec.unapply))
)

case class Foo(name: String, bd1: BinDec, bd2: BinDec)

val fooCodec: CsvCodec[Foo] = (
  (stringCodec, binDecCodec, binDecCodec)
    .imapN(Foo.apply)(Function.unlift(Foo.unapply))
)
```

Finally let's verify out CsvCodec law with an example:

```scala mdoc
val foo = Foo("foo", BinDec(10, 10), BinDec(20, 20))

val fooCsv = fooCodec.write(foo)

fooCodec.read(fooCsv)

fooCodec.read(fooCodec.write(foo)) == ((Some(foo), List()))
```

# `InvariantMonoidal` as a generalization of `Invariant`

To better understand the motivations behind the `InvariantMonoidal` type class, we show how one could naturally arrive to its definition by generalizing the concept of `Invariant` functor. This reflection is analogous to the one presented in [Free Applicative Functors by Paolo Capriotti](http://www.paolocapriotti.com/assets/applicative.pdf) to show how [`Applicative`](applicative.md) are a generalization of [`Functor`](functor.md).

Given an `Invariant[F]` instance for a certain *context* `F[_]`, its `imap` method gives a way to lift two *unary* pure functions `A => B` and `B => A` into *contextualized* functions `F[A] => F[B]`. But what about functions of other arity?

For instance, a value `a` of type `A` can be seen as a pair of nullary functions, one than given no input returns `a`, and the other than give `a` return no output, which we might want to lift them into a *contextualized* `F[A]`. Similarly, given two functions of type `(A, B) => C` and `C => (A, B)`, we might want to *contextualize* them as functions of type `(F[A], F[B]) => F[C]`.

The `Invariant` instance alone does not provide either of these lifting, and it is therefore natural to define define a type class for generalizing `Invariant`s for functions of arbitrary arity:

```scala mdoc:silent
trait MultiInvariant[F[_]] {
  def imap0[A](a: A): F[A]
  def imap1[A, B](f: A => B)(g: B => A)(fa: F[A]): F[B]
  def imap2[A, B, C](f: ((A, B)) => C)(g: C => (A, B))(fa: F[A], fb: F[B]): F[C]
}
```

Higher-arity `imapN` can be defined in terms of `imap2`, for example for `N = 3`:

```scala mdoc:silent
trait MultiInvariantImap3[F[_]] extends MultiInvariant[F] {
  def imap3[A, B, C, D](
    f: ((A, B, C)) => D,
    g: D => (A, B, C),
    fa: F[A],
    fb: F[B],
    fc: F[C]
  ): F[D] = (
    imap2[A, (B, C), D]
      (f compose { case (a, (b, c)) => (a, b, c) })
      (g andThen { case (a, b, c) => (a, (b, c)) })
      (fa, imap2[B, C, (B, C)](identity)(identity)(fb, fc))
  )
}
```

We can observe that `MultiInvariant` is none other than an alternative formulation for `InvariantMonoidal`. Indeed, `imap1` and `imap` only differ by the order of their argument, and `imap2` can easily be defined in terms of `imap` and `product`:

```scala mdoc:silent
trait Imap2FromImapProduct[F[_]] extends cats.InvariantMonoidal[F] {
  def imap2[A, B, C](f: ((A, B)) => C)(g: C => (A, B))(fa: F[A], fb: F[B]): F[C] =
    imap(product(fa, fb))(f)(g)
}
```
