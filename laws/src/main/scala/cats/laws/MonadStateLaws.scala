package cats
package laws

// Taken from http://functorial.com/psc-pages/docs/Control/Monad/State/Class/index.html
trait MonadStateLaws[F[_], S] extends MonadLaws[F] {
  implicit def FS: MonadState[F, S]
  implicit def F: Monad[F] = FS.monadInstance

  val monadStateGetIdempotent: IsEq[F[S]] =
    F.flatMap(FS.get)(_ => FS.get) <-> FS.get

  def monadStateSetTwice(s: S, t: S): IsEq[F[Unit]] =
    F.flatMap(FS.set(s))(_ => FS.set(t)) <-> FS.set(t)

  def monadStateSetGet(s: S): IsEq[F[S]] =
    F.flatMap(FS.set(s))(_ => FS.get) <-> F.flatMap(FS.set(s))(_ => F.pure(s))

  val monadStateGetSet: IsEq[F[Unit]] =
    F.flatMap(FS.get)(FS.set) <-> F.pure(())
}

object MonadStateLaws {
  def apply[F[_], S](implicit FS0: MonadState[F, S]): MonadStateLaws[F, S] =
    new MonadStateLaws[F, S] { def FS: MonadState[F, S] = FS0 }
}
