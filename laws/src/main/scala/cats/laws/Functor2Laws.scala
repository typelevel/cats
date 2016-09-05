package cats.laws

import cats.functor.Functor2
import cats.syntax.functor2._

/**
 * Laws that must be obeyed by any `Functor2`.
 */
trait Functor2Laws[F[_, _]] {
  implicit def F: Functor2[F]

  def functor2Identity[A, B](fa: F[A, B]): IsEq[F[A, B]] =
    fa.map2(identity, identity) <-> fa

  def functor2Composition[A, B, C, X, Y, Z](fa: F[A, X], f: A => B, f2: B => C, g: X => Y, g2: Y => Z): IsEq[F[C, Z]] = {
    fa.map2(f, g).map2(f2, g2) <-> fa.map2(f andThen f2, g andThen g2)
  }

  def functor2LeftMapIdentity[A, B](fa: F[A, B]): IsEq[F[A, B]] =
    fa.leftMap(identity) <-> fa

  def functor2LeftMapComposition[A, B, C, D](fa: F[A, B], f: A => C, g: C => D): IsEq[F[D, B]] = {
    fa.leftMap(f).leftMap(g) <-> fa.leftMap(f andThen g)
  }

}

object Functor2Laws {
  def apply[F[_, _]](implicit ev: Functor2[F]): Functor2Laws[F] =
    new Functor2Laws[F] {
      def F: Functor2[F] = ev
    }
}
