package cats
package laws

import cats.ContravariantSemigroupal
import cats.syntax.contravariantSemigroupal._

/**
 * Laws that are expected for any `cats.ContravariantSemigroupal`.
 */
trait ContravariantSemigroupalLaws[F[_]] extends ContravariantLaws[F] with SemigroupalLaws[F] {
  implicit override def F: ContravariantSemigroupal[F]

  def delta[A](a: A): (A, A) = (a, a)

  def contravariantSemigroupalContramap2DiagonalAssociates[A](m: F[A], n: F[A], o: F[A]): IsEq[F[A]] =
    ((m, n).contramapN(delta[A]), o).contramapN(delta[A]) <-> (m, (n, o).contramapN(delta[A])).contramapN(delta[A])
}
object ContravariantSemigroupalLaws {
  def apply[F[_]](implicit ev: ContravariantSemigroupal[F]): ContravariantSemigroupalLaws[F] =
    new ContravariantSemigroupalLaws[F] { def F: ContravariantSemigroupal[F] = ev }
}
