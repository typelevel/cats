package cats
package data

/**
 * This is a continuation transformer based on the ContT in
 * the Haskell package Control.Monad.Cont
 *
 * This is reasonably straight-forward except that to make
 * a tailRecM implementation we leverage the Defer typeclass to
 * obtain stack-safety.
 */
sealed trait ContT[M[_], A, B] extends Serializable { self =>
  def run(cb: B => M[A]): M[A]

  def map[C](fn: B => C): ContT[M, A, C] =
    ContT[M, A, C] { fn2 =>
      run(AndThen(fn).andThen(fn2))
    }

  /**
   * c.mapCont(f).run(g) == f(c.run(g))
   */
  def mapCont(fn: M[A] => M[A]): ContT[M, A, B] =
    ContT.MapCont(this, fn)

  /**
   * cont.withCont(f).run(cb) == cont.run(f(cb))
   */
  def withCont[C](fn: (C => M[A]) => B => M[A]): ContT[M, A, C] =
    ContT.WithCont(this, fn)

  def flatMap[C](fn: B => ContT[M, A, C]): ContT[M, A, C] =
    ContT[M, A, C] { fn2 =>
      val fn3: B => M[A] = AndThen(fn).andThen { cont => cont.run(fn2) }
      run(fn3)
    }
}

object ContT {

  private case class Pure[M[_], A, B](b: B) extends ContT[M, A, B] {
    def run(cb: B => M[A]): M[A] = cb(b)
  }

  private case class FromFn[M[_], A, B](fn: (B => M[A]) => M[A]) extends ContT[M, A, B] {
    def run(cb: B => M[A]): M[A] = fn(cb)
  }

  private case class WithCont[M[_], A, B, C](c0: ContT[M, A, B], fn: (C => M[A]) => B => M[A]) extends ContT[M, A, C] {
    def run(cb: C => M[A]) = c0.run(fn(cb))
  }

  private case class MapCont[M[_], A, B](c0: ContT[M, A, B], fn: M[A] => M[A]) extends ContT[M, A, B] {
    def run(cb: B => M[A]) = fn(c0.run(cb))
  }

  private case class DeferCont[M[_], A, B](next: () => ContT[M, A, B]) extends ContT[M, A, B] {
    def run(cb: B => M[A]) = {
      def loop(n: () => ContT[M, A, B]): ContT[M, A, B] =
        n() match {
          case DeferCont(n) => loop(n)
          case notDefer => notDefer
        }

      loop(next).run(cb)
    }
  }

  def pure[M[_], A, B](b: B): ContT[M, A, B] =
    Pure(b)

  def apply[M[_], A, B](fn: (B => M[A]) => M[A]): ContT[M, A, B] =
    FromFn(fn)

  def tailRecM[M[_], A, B, C](a: A)(fn: A => ContT[M, C, Either[A, B]])(implicit M: Defer[M]): ContT[M, C, B] =
    ContT[M, C, B] { cb: (B => M[C]) =>

      def go(a: A): M[C] =
        fn(a).run {
          case Left(a) => M.defer(go(a))
          case Right(b) => cb(b)
        }

      go(a)
    }

  implicit def catsDataContTDefer[M[_], B]: Defer[ContT[M, B, ?]] =
    new Defer[ContT[M, B, ?]] {
      def defer[A](c: => ContT[M, B, A]): ContT[M, B, A] =
        DeferCont(() => c)
    }

  implicit def catsDataContTMonad[M[_]: Defer, A]: Monad[ContT[M, A, ?]] =
    new Monad[ContT[M, A, ?]] {
      def pure[B](b: B): ContT[M, A, B] =
        ContT.pure(b)

      override def map[B, C](c: ContT[M, A, B])(fn: B => C): ContT[M, A, C] =
        c.map(fn)

      def flatMap[B, C](c: ContT[M, A, B])(fn: B => ContT[M, A, C]): ContT[M, A, C] =
        c.flatMap(fn)

      def tailRecM[B, C](b: B)(fn: B => ContT[M, A, Either[B, C]]): ContT[M, A, C] =
        ContT.tailRecM(b)(fn)
    }
}
