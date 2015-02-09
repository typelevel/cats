package cats
package free

import scala.annotation.tailrec

object Free {

  /** Return from the computation with the given value. */
  case class Pure[S[_], A](a: A) extends Free[S, A]

  /** Suspend the computation with the given suspension. */
  case class Suspend[S[_], A](a: S[Free[S, A]]) extends Free[S, A]

  /** Call a subroutine and continue with the given function. */
  sealed abstract class Gosub[S[_], B] extends Free[S, B] {
    type C
    val a: () => Free[S, C]
    val f: C => Free[S, B]
  }

  def gosub[S[_], A, B](a0: () => Free[S, A])(f0: A => Free[S, B]): Free[S, B] =
    new Gosub[S, B] {
      type C = A
      val a = a0
      val f = f0
    }

  type Trampoline[A] = Free[Function0, A]

  type FreeC[S[_], A] = Free[Coyoneda[S, ?], A]


  /** TODO Where to put those instances ??? */
  implicit def freeFreeLike[F[_]: Functor]: FreeLike[F, Free[F, ?]] =
    new FreeLike[F, Free[F, ?]] {
      def wrap[A](fa: F[Free[F, A]]): Free[F, A] = Suspend(fa)
    }

}

import Free._

/**
 * A free operational monad for some functor `S`. Binding is done
 * using the heap instead of the stack, allowing tail-call
 * elimination.
 */
sealed abstract class Free[S[_], A] {

  final def map[B](f: A => B): Free[S, B] =
    flatMap(a => Pure(f(a)))

  /**
   * Binds the given continuation to the result of this computation.
   * All left-associated binds are reassociated to the right.
   */
  final def flatMap[B](f: A => Free[S, B]): Free[S, B] = this match {
    case a: Gosub[S, A] => gosub(a.a)(x => gosub(() => a.f(x))(f))
    case a => gosub(() => a)(f)
  }

  /**
   * Catamorphism. Run the first given function if Pure, otherwise,
   * the second given function.
   */
  final def fold[B](r: A => B, s: S[Free[S, A]] => B)(implicit S: Functor[S]): B =
    resume.fold(s, r)

  /**
   * Evaluates a single layer of the free monad.
   */
  final def resume(implicit S: Functor[S]): (Either[S[Free[S, A]], A]) = this match {
    case Pure(a) =>
      Right(a)
    case Suspend(t) =>
      Left(t)
    case x: Gosub[S, A] =>
      x.a() match {
        case Pure(a) =>
          x.f(a).resume
        case Suspend(t) =>
          Left(S.map(t)(_ flatMap x.f))
        // The _ should be x.C, but we are hitting this bug: https://github.com/daniel-trinh/scalariform/issues/44
        case y: Gosub[S, _] =>
          y.a().flatMap(z => y.f(z) flatMap x.f).resume
      }
  }

  /**
   * Runs to completion, using a function that extracts the resumption
   * from its suspension functor.
   */
  final def go(f: S[Free[S, A]] => Free[S, A])(implicit S: Functor[S]): A = {
    @tailrec def loop(t: Free[S, A]): A =
      t.resume match {
        case Left(s) => loop(f(s))
        case Right(r) => r
      }
    loop(this)
  }

  def run(implicit S: Comonad[S]): A = go(S.extract)

  /**
   * Runs to completion, using a function that maps the resumption
   * from `S` to a monad `M`.
   */
  final def runM[M[_]](f: S[Free[S, A]] => M[Free[S, A]])(implicit S: Functor[S], M: Monad[M]): M[A] = {
    def runM2(t: Free[S, A]): M[A] = t.resume match {
      case Left(s) => Monad[M].flatMap(f(s))(runM2)
      case Right(r) => Monad[M].pure(r)
    }
    runM2(this)
  }

  /**
   * Catamorphism for `Free`.
   *
   * Runs to completion, mapping the suspension with the given transformation at each step and
   * accumulating into the monad `M`.
   */
  final def foldMap[M[_]](f: S ~> M)(implicit S: Functor[S], M: Monad[M]): M[A] =
    this.resume match {
      case Left(s) => Monad[M].flatMap(f(s))(_.foldMap(f))
      case Right(r) => Monad[M].pure(r)
    }
}

object Trampoline {
  def done[A](a: A): Trampoline[A] =
    Free.Pure[Function0,A](a)

  def suspend[A](a: => Trampoline[A]): Trampoline[A] =
    Free.Suspend[Function0, A](() => a)

  def delay[A](a: => A): Trampoline[A] =
    suspend(done(a))
}
