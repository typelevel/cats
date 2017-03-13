package cats
package laws

// Taken from http://functorial.com/psc-pages/docs/Control/Monad/Reader/Class/index.html
trait MonadReaderLaws[F[_], R] extends MonadLaws[F] {
  implicit override def F: MonadReader[F, R]

  val monadReaderAskIdempotent: IsEq[F[R]] =
    F.flatMap(F.ask)(_ => F.ask) <-> F.ask

  def monadReaderLocalAsk(f: R => R): IsEq[F[R]] =
    F.local(f)(F.ask) <-> F.map(F.ask)(f)

  def monadReaderLocalPure[A](a: A, f: R => R): IsEq[F[A]] =
    F.local(f)(F.pure(a)) <-> F.pure(a)

  def monadReaderLocalFlatMap[A, B](fra: F[A], f: A => F[B], g: R => R): IsEq[F[B]] =
    F.local(g)(F.flatMap(fra)(f)) <-> F.flatMap(F.local(g)(fra))(a => F.local(g)(f(a)))

  def monadReaderReaderAsk[A](f: R => A): IsEq[F[A]] =
    F.reader(f) <-> F.map(F.ask)(f)
}

object MonadReaderLaws {
  def apply[F[_], R](implicit FR: MonadReader[F, R]): MonadReaderLaws[F, R] =
    new MonadReaderLaws[F, R] { def F: MonadReader[F, R] = FR }
}
