package cats

import simulacrum.typeclass

/**
 * The combination of a Monad with a MonoidK
 */
@typeclass trait MonadCombine[F[_]] extends MonadFilter[F] with Alternative[F] {

  /**
   * fold over the inner structure to combining all the values with
   * our combine method inherited from MonoidK. The result is for us
   * to accumulate all of the "interesting" values of the innner G, so
   * if G is Option, we collect all the Some values, if G is Xor,
   * we collect all the Right values, etc.
   */
  def unite[G[_], A](fga: F[G[A]])(implicit G: Foldable[G]): F[A] =
    flatMap(fga) { ga =>
      G.foldLeft(ga, empty[A])((acc, a) => combine(acc, pure(a)))
    }
}
