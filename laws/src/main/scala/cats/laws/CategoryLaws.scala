package cats
package laws

import cats.arrow.Category
import cats.syntax.compose._

/**
 * Laws that must be obeyed by any `cats.arrow.Category`.
 */
trait CategoryLaws[F[_, _]] extends ComposeLaws[F] {
  implicit override def F: Category[F]

  def categoryLeftIdentity[A, B](f: F[A, B]): IsEq[F[A, B]] =
    (F.id[A].andThen(f)) <-> f

  def categoryRightIdentity[A, B](f: F[A, B]): IsEq[F[A, B]] =
    (f.andThen(F.id[B])) <-> f
}

object CategoryLaws {
  def apply[F[_, _]](implicit ev: Category[F]): CategoryLaws[F] =
    new CategoryLaws[F] { def F: Category[F] = ev }
}
