package cats
package laws

// Taken from http://functorial.com/psc-pages/docs/Control/Monad/Reader/Class/index.html
trait MonadReaderLaws[F[_], R] extends MonadLaws[F] {
  implicit def FR: MonadReader[F, R]
  implicit def F: Monad[F] = FR.monadInstance

  val monadReaderAskIdempotent: IsEq[F[R]] =
    F.flatMap(FR.ask)(_ => FR.ask) <-> FR.ask

  def monadReaderLocalAsk(f: R => R): IsEq[F[R]] =
    FR.local(f)(FR.ask) <-> F.map(FR.ask)(f)

  def monadReaderLocalPure[A](a: A, f: R => R): IsEq[F[A]] =
    FR.local(f)(F.pure(a)) <-> F.pure(a)

  def monadReaderLocalFlatMap[A, B](fra: F[A], f: A => F[B], g: R => R): IsEq[F[B]] =
    FR.local(g)(F.flatMap(fra)(f)) <-> F.flatMap(FR.local(g)(fra))(a => FR.local(g)(f(a)))
}

object MonadReaderLaws {
  def apply[F[_], R](implicit FR0: MonadReader[F, R]): MonadReaderLaws[F, R] =
    new MonadReaderLaws[F, R] { def FR: MonadReader[F, R] = FR0 }
}
