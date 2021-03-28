package cats.data

import cats.{Comonad, Functor, Representable}

/**
 * A generalization of `StoreT`, where the underlying functor `F` hasfor any `Representable` functor.
 * `Store` is the dual of `State`
 */
final case class RepresentableStore[F[_], S, A](fa: F[A], index: S)(implicit R: Representable.Aux[F, S]) {

  /**
   * Peek at what the focus would be for a given focus s.
   */
  def peek(s: S): A = R.index(fa)(s)

  /**
   * Peek at what the focus would be if the current focus where transformed
   * with the given function.
   */
  def peeks(f: S => S): A = peek(f(index))

  /**
   * Set the current focus.
   */
  def seek(s: S): RepresentableStore[F, S, A] = RepresentableStore(fa, s)

  /**
   * Modify the current focus with the given function.
   */
  def seeks(f: S => S): RepresentableStore[F, S, A] = seek(f(index))

  /**
   * Extract the value at the current index.
   */
  lazy val extract: A = peek(index)

  /**
   * `coflatten` is the dual of `flatten` on `FlatMap`. Whereas flatten removes
   * a layer of `F`, coflatten adds a layer of `F`
   */
  lazy val coflatten: RepresentableStore[F, S, RepresentableStore[F, S, A]] =
    RepresentableStore(R.tabulate(idx => RepresentableStore(fa, idx)), index)

  /**
   * `coflatMap` is the dual of `flatMap` on `FlatMap`. It applies
   * a value in a context to a function that takes a value
   * in a context and returns a normal value.
   */
  def coflatMap[B](f: RepresentableStore[F, S, A] => B): RepresentableStore[F, S, B] =
    coflatten.map(f)

  /**
   * Functor `map` for `RepresentableStore`
   */
  def map[B](f: A => B): RepresentableStore[F, S, B] =
    RepresentableStore(R.F.map(fa)(f), index)

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
  def experiment[G[_]](fn: S => G[S])(implicit G: Functor[G]): G[A] =
    G.map(fn(index))(peek)
}

object RepresentableStore {

  implicit def catsDataRepresentableStoreComonad[F[_], S](implicit
    R: Representable[F]
  ): Comonad[RepresentableStore[F, S, *]] =
    new Comonad[RepresentableStore[F, S, *]] {
      override def extract[B](x: RepresentableStore[F, S, B]): B =
        x.extract

      override def coflatMap[A, B](
        fa: RepresentableStore[F, S, A]
      )(f: RepresentableStore[F, S, A] => B): RepresentableStore[F, S, B] =
        fa.coflatten.map(f)

      override def map[A, B](fa: RepresentableStore[F, S, A])(f: A => B): RepresentableStore[F, S, B] =
        fa.map(f)
    }
}
