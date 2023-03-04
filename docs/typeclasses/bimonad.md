# Bimonad

API Documentation: @:api(cats.Bimonad)

The `Bimonad` trait directly extends `Monad` and `Comonad` without introducing new methods.  `Bimonad` is
different from other `Bi` typeclasses like `Bifunctor`, `Bifoldable` or `Bitraverse` where the prefix describes
a `F[_, _]`. The `Bimonad` is a `F[_]` and the `Bi` prefix has a different meaning here: it's both a `Monad` and a `Comonad`.  
Keep in mind `Bimonad` has its own added laws so something that is both monadic
and comonadic may not necessarily be a lawful `Bimonad`.

If you use `Bimonad` as a convenience type such that:
```scala
def f[T[_]: Monad: Comonad, S](fa: T[S]): S
```
is re-written to:
```scala
def f[T[_]: Bimonad, S](fa: T[S]): S
```
then `T[_]` also needs to respect an extra set of laws.

### NonEmptyList as a Bimonad
`NonEmptyList[_]` is a lawful `Bimonad` so you can chain computations (like a `Monad`) and `extract` the result at the end (like a `Comonad`).

Here is a possible implementation:
```scala mdoc
import cats._
import cats.data._
import cats.syntax.all._

implicit val nelBimonad =
  new Bimonad[NonEmptyList] {

    // in order to have a lawful bimonad `pure` and `extract` need to respect: `nelBimonad.extract(nelBimonad.pure(a)) <-> a`
    override def pure[A](a: A): NonEmptyList[A] =
      NonEmptyList.one(a)

    override def extract[A](fa: NonEmptyList[A]): A =
      fa.head

    // use coflatMap from NonEmptyList
    override def coflatMap[A, B](fa: NonEmptyList[A])(f: NonEmptyList[A] => B): NonEmptyList[B] =
      fa.coflatMap(f)

    // use flatMap from NonEmptyList
    override def flatMap[A, B](fa: NonEmptyList[A])(f: A => NonEmptyList[B]): NonEmptyList[B] =
      fa.flatMap(f)

    // the tailRecM implementation is not the subject of this material
    // as an exercise try to implement it yourself
    override def tailRecM[A, B](a: A)(fn: A => NonEmptyList[Either[A, B]]): NonEmptyList[B] =
      ???
  }
```

Note the equivalence:
```scala mdoc
nelBimonad.pure(true).extract === NonEmptyList.one(true).head
```

Using generic bimonad syntax we could define a function that appends and extracts a configuration:
```scala mdoc
def make[T[_]: Bimonad](config: T[String]): String = 
  config
    .flatMap(c => Bimonad[T].pure(c + " with option A"))
    .flatMap(c => Bimonad[T].pure(c + " with option B"))
    .flatMap(c => Bimonad[T].pure(c + " with option C"))
    .extract
```

This works with one element non-empty lists:
```scala mdoc
make(NonEmptyList.one("config"))
```

`Function0[_]` and `Eval[_]` are also lawful bimonads so the following calls are also valid:
```scala mdoc
make(() => "config")

make(Eval.later("config"))
```
