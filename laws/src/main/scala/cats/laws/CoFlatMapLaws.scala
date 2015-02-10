package cats.laws

import cats.CoFlatMap
import cats.data.Cokleisli
import cats.syntax.coflatMap._

/**
 * Laws that must be obeyed by any [[CoFlatMap]].
 */
class CoFlatMapLaws[F[_]](implicit F: CoFlatMap[F]) extends FunctorLaws[F] {
  def coFlatMapAssociativity[A, B, C](fa: F[A], f: F[A] => B, g: F[B] => C): (F[C], F[C]) =
    fa.coflatMap(f).coflatMap(g) -> fa.coflatMap(x => g(x.coflatMap(f)))

  /**
   * The composition of [[cats.data.Cokleisli]] arrows is associative. This is
   * analogous to the associativity law of [[CoFlatMap.coflatMap]].
   */
  def cokleisliAssociativity[A, B, C, D](f: F[A] => B, g: F[B] => C, h: F[C] => D, fa: F[A]): (D, D) = {
    val (cf, cg, ch) = (Cokleisli(f), Cokleisli(g), Cokleisli(h))
    (cf compose (cg compose ch)).runCokleisli(fa) -> ((cf compose cg) compose ch).runCokleisli(fa)
  }
}
