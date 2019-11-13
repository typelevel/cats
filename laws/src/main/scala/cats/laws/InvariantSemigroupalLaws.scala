package cats
package laws

/**
 * Laws that are expected for any `cats.InvariantSemigroupal`.
 */
trait InvariantSemigroupalLaws[F[_]] extends InvariantLaws[F] with SemigroupalLaws[F] {
  implicit override def F: InvariantSemigroupal[F]
  import cats.syntax.semigroupal._
  import cats.syntax.invariant._

  def invariantSemigroupalAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): IsEq[F[(A, (B, C))]] =
    fa.product(fb.product(fc)) <-> fa
      .product(fb)
      .product(fc)
      .imap { case ((a, b), c) => (a, (b, c)) } { case (a, (b, c)) => ((a, b), c) }
}

object InvariantSemigroupalLaws {
  def apply[F[_]](implicit ev: InvariantSemigroupal[F]): InvariantSemigroupalLaws[F] =
    new InvariantSemigroupalLaws[F] { def F: InvariantSemigroupal[F] = ev }
}
