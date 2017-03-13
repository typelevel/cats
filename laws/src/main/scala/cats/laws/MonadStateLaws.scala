package cats
package laws

// Taken from http://functorial.com/psc-pages/docs/Control/Monad/State/Class/index.html
trait MonadStateLaws[F[_], S] extends MonadLaws[F] {
  implicit override def F: MonadState[F, S]

  val monadStateGetIdempotent: IsEq[F[S]] =
    F.flatMap(F.get)(_ => F.get) <-> F.get

  def monadStateSetTwice(s: S, t: S): IsEq[F[Unit]] =
    F.flatMap(F.set(s))(_ => F.set(t)) <-> F.set(t)

  def monadStateSetGet(s: S): IsEq[F[S]] =
    F.flatMap(F.set(s))(_ => F.get) <-> F.flatMap(F.set(s))(_ => F.pure(s))

  val monadStateGetSet: IsEq[F[Unit]] =
    F.flatMap(F.get)(F.set) <-> F.pure(())
}

object MonadStateLaws {
  def apply[F[_], S](implicit FS: MonadState[F, S]): MonadStateLaws[F, S] =
    new MonadStateLaws[F, S] { def F: MonadState[F, S] = FS }
}
