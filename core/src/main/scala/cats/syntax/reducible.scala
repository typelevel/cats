package cats
package syntax

trait ReducibleSyntax extends Reducible.ToReducibleOps {
  implicit final def catsSyntaxNestedReducible[F[_]: Reducible, G[_], A](fga: F[G[A]]): NestedReducibleOps[F, G, A] =
    new NestedReducibleOps[F, G, A](fga)
}

final class NestedReducibleOps[F[_], G[_], A](private val fga: F[G[A]]) extends AnyVal {
  def reduceK(implicit F: Reducible[F], G: SemigroupK[G]): G[A] = F.reduceK(fga)
}

trait ReducibleSyntaxBinCompat0 {
  implicit final def catsSyntaxReducibleOps0[F[_], A](fa: F[A]): ReducibleOps0[F, A] =
    new ReducibleOps0[F, A](fa)
}

final class ReducibleOps0[F[_], A](val fa: F[A]) extends AnyVal {

  /**
   * Apply `f` to each element of `fa` and combine them using the
   * given `SemigroupK[G]`.
   *
   * scala>import cats._, cats.implicits._
   * scala>val f: Int => Endo[String] = i => (s => s + i)
   * scala>val x: Endo[String] = NonEmptyList(1, 2, 3).reduceMapK(f)
   * scala>val a = x("foo")
   * a: String = "foo321"
   * */
  def reduceMapK[G[_], B](f: A => G[B])(implicit F: Reducible[F], G: SemigroupK[G]): G[B] =
    F.reduceLeftTo(fa)(f)((b, a) => G.combineK(b, f(a)))
}
