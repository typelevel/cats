package cats

import simulacrum.typeclass

@typeclass trait Unfoldable[F[_]] { self =>

  /**
    * Build an F[A] from a `seed` and a generating function `f``
    * The generating function `f` behave as follows:
    * - If `f(b)` is `None`, then `unfoldLeft(b)(f)` should be empty.
    * - If `f(b)` is `Some((b1, a))`, then `unfoldLeft(b)(f)` should consist of `a` appended by `unfoldLeft(b1)(f)`
    */
  def unfoldLeft[A, B](seed: B)(f: B => Option[(B, A)]): F[A]

  def none[A]: F[A] =
    Unfoldable.DefaultImpl.none(this)

  def singleton[A](value: A): F[A] =
    Unfoldable.DefaultImpl.singleton(value)(this)

  def replicate[A](n: Int)(value: A): F[A] =
    Unfoldable.DefaultImpl.replicate(n)(value)(this)

  def build[A](as: A*): F[A] =
    Unfoldable.DefaultImpl.build(as: _*)(this)

  def fromFoldable[G[_], A](ga: G[A])(implicit G: Foldable[G]): F[A] =
    Unfoldable.DefaultImpl.fromFoldable(ga)(this, G)

}

object Unfoldable {

  object DefaultImpl {

    def none[F[_], A](implicit F: Unfoldable[F]): F[A] =
      F.unfoldLeft(())(_ => None)

    def singleton[F[_], A](value: A)(implicit F: Unfoldable[F]): F[A] =
      replicate(1)(value)

    def replicate[F[_], A](n: Int)(value: A)(implicit F: Unfoldable[F]): F[A] =
      F.unfoldLeft(n)(i => if(i <= 0) None else Some((i - 1, value)))

    def build[F[_], A](as: A*)(implicit F: Unfoldable[F]): F[A] =
      fromList(as.toList)

    def fromFoldable[F[_], G[_], A](ga: G[A])(implicit F: Unfoldable[F], G: Foldable[G]): F[A] =
      fromList(G.toList(ga))

    def fromList[F[_], A](ga: List[A])(implicit F: Unfoldable[F]): F[A] = {
      def go(l: List[A]): Option[(List[A], A)] = l match {
        case Nil     => None
        case x :: xs => Some((xs, x))
      }
      F.unfoldLeft(ga.reverse)(go)
    }
  }
}
