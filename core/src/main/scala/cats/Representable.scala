package cats

/**
 * Representable.
 *
 * Is a witness to the isomorphism forall A. F[A] <-> Representation => A
 *
 * Must obey the laws defined in cats.laws.RepresentableLaws
 * i.e.
 * tabulate andThen index = identity
 * index andThen tabulate = identity
 *
 * Inspired by the Haskell representable package
 * http://hackage.haskell.org/package/representable-functors-3.2.0.2/docs/Data-Functor-Representable.html
 */
trait Representable[F[_]] extends Serializable {

  def F: Functor[F]

  type Representation

  /**
   * Create a function that "indexes" into the `F` structure using `Representation`
   */
  def index[A](f: F[A]): Representation => A

  /**
   * Reconstructs the `F` structure using the index function
   */
  def tabulate[A](f: Representation => A): F[A]
}

private trait RepresentableMonad[F[_], R] extends Monad[F] {

  def R: Representable.Aux[F, R]

  override def pure[A](x: A): F[A] = R.tabulate(_ => x)

  override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    R.tabulate(a => R.index(f(R.index(fa)(a)))(a))

  override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = {
    R.tabulate { r: R =>
      @annotation.tailrec
      def loop(a: A): B =
        R.index(f(a))(r) match {
          case Right(b) => b
          case Left(a) => loop(a)
        }

      loop(a)
    }
  }
}

private trait RepresentableBimonad[F[_], R] extends RepresentableMonad[F, R] with Bimonad[F] {

  def M: Monoid[R]

  override def coflatMap[A, B](w: F[A])(f: F[A] => B): F[B] =
    R.tabulate(m => f(R.tabulate(x => R.index(w)(M.combine(m, x)))))

  override def extract[A](fa: F[A]): A =
    R.index(fa)(M.empty)
}

object Representable {
  type Aux[F[_], R] = Representable[F] { type Representation = R }

  /**
   * Summon the `Representable` instance for `F`
   */
  def apply[F[_]](implicit ev: Representable[F]): Representable[F] = ev

  /**
   * Derives a `Monad` instance for any `Representable` functor
   */
  def monad[F[_]](implicit Rep: Representable[F]): Monad[F] = new RepresentableMonad[F, Rep.Representation] {
    override def R: Representable.Aux[F, Rep.Representation] = Rep
  }

  /**
   * Derives a `Bimonad` instance for any `Representable` functor whos representation
   * has a `Monoid` instance.
   */
  def bimonad[F[_], R](implicit Rep: Representable.Aux[F, R], Mon: Monoid[R]): Bimonad[F] = new RepresentableBimonad[F, R] {
    override def R: Representable.Aux[F, R] = Rep
    override def M: Monoid[R] = Mon
  }
}
