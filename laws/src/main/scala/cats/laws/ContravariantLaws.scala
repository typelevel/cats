package cats
package laws

import cats.Contravariant
import cats.syntax.contravariant._

/**
 * Laws that must be obeyed by any `cats.Contravariant`.
 */
trait ContravariantLaws[F[_]] extends InvariantLaws[F] {
  implicit override def F: Contravariant[F]

  def contravariantIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.contramap(identity[A]) <-> fa

  def contravariantComposition[A, B, C](fa: F[A], f: B => A, g: C => B): IsEq[F[C]] =
    fa.contramap(f).contramap(g) <-> fa.contramap(f.compose(g))
}

object ContravariantLaws {
  def apply[F[_]](implicit ev: Contravariant[F]): ContravariantLaws[F] =
    new ContravariantLaws[F] { def F: Contravariant[F] = ev }
}
