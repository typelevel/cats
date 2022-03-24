package cats
package laws

/**
 * Laws that must be obeyed by any `cats.MonoidK`.
 */
trait MonoidKLaws[F[_]] extends SemigroupKLaws[F] {
  implicit override def F: MonoidK[F]

  def monoidKLeftIdentity[A](a: F[A]): IsEq[F[A]] =
    F.combineK(F.empty, a) <-> a

  def monoidKRightIdentity[A](a: F[A]): IsEq[F[A]] =
    F.combineK(a, F.empty) <-> a

  def repeatK0[A](x: F[A]): IsEq[F[A]] =
    F.combineNK(x, 0) <-> F.empty

  def collectK0[A](x: A): IsEq[F[A]] =
    F.combineAllK(Nil) <-> F.empty

  def combineAllK[A](xs: Vector[F[A]]): IsEq[F[A]] =
    F.combineAllK(xs) <-> (F.empty[A] +: xs).reduce(F.combineK[A])

  def isId[A](x: F[A], eqv: Eq[F[A]]): IsEq[Boolean] =
    eqv.eqv(x, F.empty) <-> F.isEmpty(x)(eqv)
}

object MonoidKLaws {
  def apply[F[_]](implicit ev: MonoidK[F]): MonoidKLaws[F] =
    new MonoidKLaws[F] { def F: MonoidK[F] = ev }
}
