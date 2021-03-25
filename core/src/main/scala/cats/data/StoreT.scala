package cats.data

import cats.{Comonad, Functor}

/*
 * The dual of `StateT`
 */
final case class StoreT[F[_], S, A](runF: F[S => A], index: S) {

  /**
   * Get the current index.
   */
  def pos: S = index

  /**
   * Peek at what the focus would be for a different focus.
   */
  def peek(s: S)(implicit F: Functor[F]): F[A] = F.map(runF)(_.apply(index))

  /**
   * Peek at what the focus would be if the current focus where transformed
   * with the given function.
   */
  def peeks(f: S => S)(implicit F: Functor[F]): F[A] = peek(f(index))

  /*
   * Set the current focus.
   */
  def seek(s: S): StoreT[F, S, A] = StoreT(runF, s)

  /*
   * Modify the current focus with the given function.
   */
  def seeks(f: S => S): StoreT[F, S, A] = seek(f(index))

  /**
   * Extract the focus at the current index.
   */
  def extract(implicit F: Comonad[F]): A = F.extract(peek(index))

  def coflatMap[B](f: StoreT[F, S, A] => B)(implicit F: Comonad[F]): StoreT[F, S, B] = StoreT(
    F.map(F.coflatten(runF))((x: F[S => A]) => (s: S) => f(StoreT(x, s))),
    index
  )

  def coflatten(implicit F: Comonad[F]): StoreT[F, S, StoreT[F, S, A]] =
    StoreT(
      F.map(F.coflatten(runF))((x: F[S => A]) => (s: S) => StoreT(x, s)),
      index
    )

  def map[B](g: A => B)(implicit F: Functor[F]): StoreT[F, S, B] = StoreT(
    F.map(runF)((f: S => A) => AndThen(f).andThen(g(_))),
    index
  )

}

object StoreT extends StoreTInstances1 {
  implicit def comonadForStoreT[F[_]: Comonad, S]: Comonad[StoreT[F, S, *]] = new Comonad[StoreT[F, S, *]] {

    override def map[A, B](fa: StoreT[F, S, A])(f: A => B): StoreT[F, S, B] = fa.map(f)

    override def coflatMap[A, B](fa: StoreT[F, S, A])(f: StoreT[F, S, A] => B): StoreT[F, S, B] =
      fa.coflatMap(f)

    override def extract[A](fa: StoreT[F, S, A]): A = fa.extract

  }
}

trait StoreTInstances1 {
  implicit def functorForStoreT[F[_]: Functor, S]: Functor[StoreT[F, S, *]] = new Functor[StoreT[F, S, *]] {

    override def map[A, B](fa: StoreT[F, S, A])(f: A => B): StoreT[F, S, B] = fa.map(f)

  }
}
