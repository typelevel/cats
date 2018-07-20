package cats.data

import cats.{Comonad, Functor, Representable}

/**
 * A generalisation of the Store comonad, for any `Representable` functor.
 * `Store` is the dual of `State`
 */
final case class RepresentableStore[F[_], S, A](fa: F[A], index: S)(implicit R: Representable.Aux[F, S]) {
  /**
   * Inspect the value at "index" s
   */
  def peek(s: S): A = R.index(fa)(s)

  /**
   * Extract the value at the current index.
   */
  lazy val extract: A = peek(index)

  /**
   * Duplicate the store structure
   */
  lazy val coflatten: RepresentableStore[F, S, RepresentableStore[F, S, A]] =
    RepresentableStore(R.tabulate(idx => RepresentableStore(fa, idx)), index)

  def map[B](f: A => B): RepresentableStore[F, S, B] = {
    RepresentableStore(R.F.map(fa)(f), index)
  }

  /**
   * Given a functorial computation on the index `S` peek at the value in that functor.
   *
   * {{{
   *   import cats._, implicits._, data.Store
   *
   *   val initial = List("a", "b", "c")
   *   val store = Store(idx => initial.get(idx).getOrElse(""), 0)
   *   val adjacent = store.experiment[List] { idx => List(idx - 1, idx, idx + 1) }
   *
   *   require(adjacent == List("", "a", "b"))
   * }}}
   */
  def experiment[G[_]](fn: S => G[S])(implicit G: Functor[G]): G[A] = {
    G.map(fn(index))(peek)
  }
}

object RepresentableStore {

  implicit def catsDataRepresentableStoreComonad[F[_], S](implicit R: Representable[F]): Comonad[RepresentableStore[F, S, ?]] =
    new Comonad[RepresentableStore[F, S, ?]] {
      override def extract[B](x: RepresentableStore[F, S, B]): B =
        x.extract

      override def coflatMap[A, B](fa: RepresentableStore[F, S, A])(f: RepresentableStore[F, S, A] => B): RepresentableStore[F, S, B] =
        fa.coflatten.map(f)

      override def map[A, B](fa: RepresentableStore[F, S, A])(f: A => B): RepresentableStore[F, S, B] =
        fa.map(f)
    }
}
