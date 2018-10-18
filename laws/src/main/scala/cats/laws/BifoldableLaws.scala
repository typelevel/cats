package cats
package laws

trait BifoldableLaws[F[_, _]] {
  implicit def F: Bifoldable[F]

  def bifoldLeftConsistentWithBifoldMap[A, B, C](fab: F[A, B], f: A => C, g: B => C)(implicit C: Monoid[C]): IsEq[C] = {
    val expected = F.bifoldLeft(fab, C.empty)(
      (c: C, a: A) => C.combine(c, f(a)),
      (c: C, b: B) => C.combine(c, g(b))
    )
    expected <-> F.bifoldMap(fab)(f, g)
  }

  def bifoldRightConsistentWithBifoldMap[A, B, C](fab: F[A, B], f: A => C, g: B => C)(
    implicit C: Monoid[C]
  ): IsEq[C] = {
    val expected = F.bifoldRight(fab, Later(C.empty))(
      (a: A, ec: Eval[C]) => ec.map(c => C.combine(f(a), c)),
      (b: B, ec: Eval[C]) => ec.map(c => C.combine(g(b), c))
    )
    expected.value <-> F.bifoldMap(fab)(f, g)
  }
}

object BifoldableLaws {
  def apply[F[_, _]](implicit ev: Bifoldable[F]): BifoldableLaws[F] =
    new BifoldableLaws[F] {
      def F: Bifoldable[F] = ev
    }
}
