package cats
package laws

trait Foldable2Laws[F[_, _]] {
  implicit def F: Foldable2[F]

  def fold2LeftConsistentWithFold2Map[A, B, C](fab: F[A, B], f: A => C, g: B => C)(implicit C: Monoid[C]): IsEq[C] = {
    val expected = F.fold2Left(fab, C.empty)(
      (c: C, a: A) => C.combine(c, f(a)),
      (c: C, b: B) => C.combine(c, g(b))
    )
    expected <-> F.fold2Map(fab)(f, g)
  }

  def fold2RightConsistentWithFold2Map[A, B, C](fab: F[A, B], f: A => C, g: B => C)(implicit C: Monoid[C]): IsEq[C] = {
    val expected = F.fold2Right(fab, Later(C.empty))(
      (a: A, ec: Eval[C]) => ec.map(c => C.combine(f(a), c)),
      (b: B, ec: Eval[C]) => ec.map(c => C.combine(g(b), c))
    )
    expected.value <-> F.fold2Map(fab)(f, g)
  }
}

object Foldable2Laws {
  def apply[F[_, _]](implicit ev: Foldable2[F]): Foldable2Laws[F] =
    new Foldable2Laws[F] {
      def F: Foldable2[F] = ev
    }
}
