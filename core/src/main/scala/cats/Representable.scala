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
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> type Pair[A] = (A, A)
   *
   * scala> val indexed: Boolean => String = Representable[Pair].index(("foo", "bar"))
   *
   * scala> indexed(true)
   * res0: String = foo
   *
   * scala> indexed(false)
   * res1: String = bar
   * }}}
   */
  def index[A](f: F[A]): Representation => A

  /**
   * Reconstructs the `F` structure using the index function
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> type Pair[A] = (A, A)
   *
   * scala> val f: Boolean => String = {
   *      | case true => "foo"
   *      | case false => "bar"
   *      | }
   *
   * scala> f.tabulate[Pair]
   * res0: Pair[String] = (foo,bar)
   * }}}
   */
  def tabulate[A](f: Representation => A): F[A]
}

private trait RepresentableMonad[F[_], R] extends Monad[F] {

  def R: Representable.Aux[F, R]

  override def pure[A](x: A): F[A] = R.tabulate(_ => x)

  override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    R.tabulate(a => R.index(f(R.index(fa)(a)))(a))

  override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
    R.tabulate { (r: R) =>
      @annotation.tailrec
      def loop(a: A): B =
        R.index(f(a))(r) match {
          case Right(b) => b
          case Left(a)  => loop(a)
        }

      loop(a)
    }
}

private trait RepresentableBimonad[F[_], R] extends RepresentableMonad[F, R] with Bimonad[F] {

  def M: Monoid[R]

  override def coflatMap[A, B](w: F[A])(f: F[A] => B): F[B] =
    R.tabulate(m => f(R.tabulate(x => R.index(w)(M.combine(m, x)))))

  override def extract[A](fa: F[A]): A =
    R.index(fa)(M.empty)
}

private trait RepresentableDistributive[F[_], R] extends Distributive[F] {

  def R: Representable.Aux[F, R]

  override def distribute[G[_], A, B](ga: G[A])(f: A => F[B])(implicit G: Functor[G]): F[G[B]] =
    R.tabulate(r => G.map(ga)(a => R.index(f(a))(r)))

  override def map[A, B](fa: F[A])(f: A => B): F[B] = R.F.map(fa)(f)
}

object Representable {
  type Aux[F[_], R] = Representable[F] { type Representation = R }

  /**
   * Summon the `Representable` instance for `F`
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> type Pair[A] = (A, A)
   *
   * scala> Representable[Pair].index(("foo", "bar"))(false)
   * res0: String = bar
   * }}}
   */
  def apply[F[_]](implicit ev: Representable[F]): Representable.Aux[F, ev.Representation] = ev

  /**
   * Derives a `Monad` instance for any `Representable` functor
   */
  def monad[F[_]](implicit Rep: Representable[F]): Monad[F] = new RepresentableMonad[F, Rep.Representation] {
    override def R: Representable.Aux[F, Rep.Representation] = Rep
  }

  /**
   * Derives a `Bimonad` instance for any `Representable` functor whose representation
   * has a `Monoid` instance.
   */
  def bimonad[F[_], R](implicit Rep: Representable.Aux[F, R], Mon: Monoid[R]): Bimonad[F] =
    new RepresentableBimonad[F, R] {
      override def R: Representable.Aux[F, R] = Rep
      override def M: Monoid[R] = Mon
    }

  /**
   * Derives a `Distributive` instance for any `Representable` functor
   */
  def distributive[F[_]](implicit Rep: Representable[F]): Distributive[F] =
    new RepresentableDistributive[F, Rep.Representation] {
      override def R: Aux[F, Rep.Representation] = Rep
    }
}
