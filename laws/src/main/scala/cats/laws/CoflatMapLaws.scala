package cats
package laws

import cats.data.Cokleisli
import cats.implicits._

/**
 * Laws that must be obeyed by any `CoflatMap`.
 */
trait CoflatMapLaws[F[_]] extends FunctorLaws[F] {
  implicit override def F: CoflatMap[F]

  def coflatMapAssociativity[A, B, C](fa: F[A], f: F[A] => B, g: F[B] => C): IsEq[F[C]] =
    fa.coflatMap(f).coflatMap(g) <-> fa.coflatMap(x => g(x.coflatMap(f)))

  def coflattenThroughMap[A](fa: F[A]): IsEq[F[F[F[A]]]] =
    fa.coflatten.coflatten <-> fa.coflatten.map(_.coflatten)

  def coflattenCoherence[A, B](fa: F[A], f: F[A] => B): IsEq[F[B]] =
    fa.coflatMap(f) <-> fa.coflatten.map(f)

  def coflatMapIdentity[A, B](fa: F[A]): IsEq[F[F[A]]] =
    fa.coflatten <-> fa.coflatMap(identity)

  /**
   * The composition of `cats.data.Cokleisli` arrows is associative. This is
   * analogous to [[coflatMapAssociativity]].
   */
  def cokleisliAssociativity[A, B, C, D](f: F[A] => B, g: F[B] => C, h: F[C] => D, fa: F[A]): IsEq[D] = {
    val (cf, cg, ch) = (Cokleisli(f), Cokleisli(g), Cokleisli(h))
    (cf.andThen(cg)).andThen(ch).run(fa) <-> cf.andThen(cg.andThen(ch)).run(fa)
  }
}

object CoflatMapLaws {
  def apply[F[_]](implicit ev: CoflatMap[F]): CoflatMapLaws[F] =
    new CoflatMapLaws[F] { def F: CoflatMap[F] = ev }
}
