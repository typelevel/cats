package cats
package laws

/**
 * Laws that must be obeyed by any `cats.InvariantMonoidal`.
 */
trait InvariantMonoidalLaws[F[_]] extends InvariantSemigroupalLaws[F] {
  override implicit def F: InvariantMonoidal[F]
  import cats.syntax.semigroupal._
  import cats.syntax.invariant._

  def invariantMonoidalLeftIdentity[A, B](fa: F[A]): IsEq[F[A]] =
    F.unit.product(fa).imap(_._2)(a => ((), a)) <-> fa

  def invariantMonoidalRightIdentity[A, B](fa: F[A]): IsEq[F[A]] =
    fa.product(F.unit).imap(_._1)(a => (a, ())) <-> fa

  def invariantMonoidalAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]):
    IsEq[F[(A, (B, C))]] =
      fa.product(fb.product(fc)) <-> fa.product(fb).product(fc)
        .imap { case ((a, b), c) => (a, (b, c)) } { case (a, (b, c)) => ((a, b), c) }
}

object InvariantMonoidalLaws {
  def apply[F[_]](implicit i: InvariantMonoidal[F]): InvariantMonoidalLaws[F] =
    new InvariantMonoidalLaws[F] { def F: InvariantMonoidal[F] = i }
}
