package cats
package laws

import functor.Contravariant

trait MonoidalLaws[F[_]] {

  implicit def F: Monoidal[F] with Functor[F]

  def covariantProductAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): IsEq[F[(A, B, C)]] =
    F.map(F.product(fa, F.product(fb, fc))) { case (a, (b, c)) => (a, b, c) } <-> F.map(F.product(F.product(fa, fb), fc)) { case ((a, b), c) => (a, b, c) }

}

trait ContravariantMonoidalLaws[F[_]] {

  implicit def F: Monoidal[F] with Contravariant[F]

  def contravariantProductAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): IsEq[F[(A, B, C)]] =
    F.contramap[(A, (B, C)), (A, B, C)](F.product(fa, F.product(fb, fc))) { case (a, b, c) => (a, (b, c)) } <-> F.contramap[((A, B), C), (A, B, C)](F.product(F.product(fa, fb), fc)) { case (a, b, c) => ((a, b), c) }

}

object MonoidalLaws {

  def covariant[F[_]](implicit ev: Monoidal[F] with Functor[F]): MonoidalLaws[F] =
    new MonoidalLaws[F] { val F: Monoidal[F] with Functor[F] = ev }

  def contravariant[F[_]](implicit ev: Monoidal[F] with Contravariant[F]): ContravariantMonoidalLaws[F] =
    new ContravariantMonoidalLaws[F] { val F: Monoidal[F] with Contravariant[F] = ev }

}