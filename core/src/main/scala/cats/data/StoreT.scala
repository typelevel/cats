package cats.data

import cats.{Applicative, Comonad, Functor, Monoid}

/*
 * The dual of `StateT`
 */
final case class StoreT[F[_], S, A](runF: F[S => A], index: S) {

  /**
   * Get the current index.
   */
  val pos: S = index

  def run(implicit F: Functor[F]) = F.map(runF)(_.apply(index))

  /**
   * Peek at what the focus would be for a different focus.
   */
  def peek(s: S)(implicit F: Comonad[F]): A = F.extract(F.map(runF)(_.apply(index)))

  /**
   * Peek at what the focus would be if the current focus where transformed
   * with the given function.
   */
  def peeks(f: S => S)(implicit F: Comonad[F]): A = peek(f(index))

  /**
   * Set the current focus.
   */
  def seek(s: S): StoreT[F, S, A] = StoreT(runF, s)

  /**
   * Modify the current focus with the given function.
   */
  def seeks(f: S => S): StoreT[F, S, A] = seek(f(index))

  /**
   * Extract the focus at the current index.
   */
  def extract(implicit F: Comonad[F]): A = peek(index)

  def coflatMap[B](f: StoreT[F, S, A] => B)(implicit F: Comonad[F]): StoreT[F, S, B] = StoreT(
    F.map(F.coflatten(runF))((x: F[S => A]) => (s: S) => f(StoreT(x, s))),
    index
  )

  /**
   * `coflatMap` is the dual of `flatMap` on `FlatMap`. It applies
   * a value in a context to a function that takes a value
   * in a context and returns a normal value.
   */
  def coflatten(implicit F: Comonad[F]): StoreT[F, S, StoreT[F, S, A]] =
    StoreT(
      F.map(F.coflatten(runF))((x: F[S => A]) => (s: S) => StoreT(x, s)),
      index
    )

  /**
   * `coflatten` is the dual of `flatten` on `FlatMap`. Whereas flatten removes
   * a layer of `F`, coflatten adds a layer of `F`
   */
  def map[B](g: A => B)(implicit F: Functor[F]): StoreT[F, S, B] = StoreT(
    F.map(runF)((f: S => A) => AndThen(f).andThen(g(_))),
    index
  )

  /**
   * Given a functorial computation on the index `S` peek at the value in that functor.
   */
  def experiment[G[_]](f: S => G[S])(implicit F: Comonad[F], G: Functor[G]): G[A] =
    G.map(f(index))(peek(_))

}

object StoreT extends StoreTInstances1 {

  def pure[F[_], S, A](x: A)(implicit F: Applicative[F], S: Monoid[S]): StoreT[F, S, A] =
    StoreT(F.pure((_: S) => x), S.empty)

  implicit def comonadForStoreT[F[_]: Comonad, S]: Comonad[StoreT[F, S, *]] = new Comonad[StoreT[F, S, *]] {

    override def map[A, B](fa: StoreT[F, S, A])(f: A => B): StoreT[F, S, B] = fa.map(f)

    override def coflatMap[A, B](fa: StoreT[F, S, A])(f: StoreT[F, S, A] => B): StoreT[F, S, B] =
      fa.coflatMap(f)

    override def extract[A](fa: StoreT[F, S, A]): A = fa.extract

  }
}

trait StoreTInstances1 extends StoreTInstances2 {
  implicit def applicativeForStoreT[F[_], S](implicit F: Applicative[F], S: Monoid[S]): Applicative[StoreT[F, S, *]] =
    new Applicative[StoreT[F, S, *]] {

      def pure[A](x: A): StoreT[F, S, A] = StoreT.pure[F, S, A](x)

      def ap[A, B](ff: StoreT[F, S, A => B])(fa: StoreT[F, S, A]): StoreT[F, S, B] = StoreT(
        F.map(F.tuple2(ff.runF, fa.runF)) { case (f, a) =>
          (s: S) => f(s).apply(a(s))
        },
        S.combine(ff.index, fa.index)
      )

    }
}

trait StoreTInstances2 {

  implicit def functorForStoreT[F[_]: Functor, S]: Functor[StoreT[F, S, *]] = new Functor[StoreT[F, S, *]] {

    override def map[A, B](fa: StoreT[F, S, A])(f: A => B): StoreT[F, S, B] = fa.map(f)

  }
}
