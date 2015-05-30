package cats
package laws

trait FoldableLaws[F[_]] {
  implicit def F: Foldable[F]

  def leftFoldConsistentWithFoldMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit
    M: Monoid[B]
  ): IsEq[B] = {
    F.foldMap(fa)(f) <-> F.foldLeft(fa, M.empty){ (b, a) => M.combine(b, f(a)) }
  }

  def rightFoldConsistentWithFoldMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit
    M: Monoid[B]
  ): IsEq[B] = {
    F.foldMap(fa)(f) <-> F.foldRight(fa, Lazy(M.empty)){ a => Fold.Continue(M.combine(_, f(a)))}.value
  }
}

object FoldableLaws {
  def apply[F[_]](implicit ev: Foldable[F]): FoldableLaws[F] =
    new FoldableLaws[F] { def F: Foldable[F] = ev }
}
