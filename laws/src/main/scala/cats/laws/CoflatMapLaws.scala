package cats.laws

import cats.CoflatMap
import cats.data.Cokleisli
import cats.syntax.coflatMap._

/**
 * Laws that must be obeyed by any [[CoflatMap]].
 */
trait CoflatMapLaws[F[_]] extends FunctorLaws[F] {
  implicit override def F: CoflatMap[F]

  def coFlatMapAssociativity[A, B, C](fa: F[A], f: F[A] => B, g: F[B] => C): IsEq[F[C]] =
    fa.coflatMap(f).coflatMap(g) <-> fa.coflatMap(x => g(x.coflatMap(f)))

  /**
   * The composition of [[cats.data.Cokleisli]] arrows is associative. This is
   * analogous to the associativity law of [[CoflatMap.coflatMap]].
   */
  def cokleisliAssociativity[A, B, C, D](f: F[A] => B, g: F[B] => C, h: F[C] => D, fa: F[A]): IsEq[D] = {
    val (cf, cg, ch) = (Cokleisli(f), Cokleisli(g), Cokleisli(h))
    ((ch compose cg) compose cf).run(fa) <-> (ch compose (cg compose cf)).run(fa)
  }
}

object CoflatMapLaws {
  def apply[F[_]](implicit ev: CoflatMap[F]): CoflatMapLaws[F] =
    new CoflatMapLaws[F] { def F = ev }
}
