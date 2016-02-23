package cats
package free

import scala.annotation.tailrec

import cats.data.Xor, Xor.{Left, Right}
import cats.arrow.NaturalTransformation

object Free {
  /**
   * Return from the computation with the given value.
   */
  private final case class Pure[S[_], A](a: A) extends Free[S, A]

  /** Suspend the computation with the given suspension. */
  private final case class Suspend[S[_], A](a: S[A]) extends Free[S, A]

  /** Call a subroutine and continue with the given function. */
  private final case class Gosub[S[_], B, C](c: Free[S, C], f: C => Free[S, B]) extends Free[S, B]

  /**
   * Suspend a value within a functor lifting it to a Free.
   */
  def liftF[F[_], A](value: F[A]): Free[F, A] = Suspend(value)

  /** Suspend the Free with the Applicative */
  def suspend[F[_], A](value: => Free[F, A])(implicit F: Applicative[F]): Free[F, A] =
    liftF(F.pure(())).flatMap(_ => value)

  /** Lift a pure value into Free */
  def pure[S[_], A](a: A): Free[S, A] = Pure(a)

  final class FreeInjectPartiallyApplied[F[_], G[_]] private[free] {
    def apply[A](fa: F[A])(implicit I : Inject[F, G]): Free[G, A] =
      Free.liftF(I.inj(fa))
  }

  def inject[F[_], G[_]]: FreeInjectPartiallyApplied[F, G] = new FreeInjectPartiallyApplied

  /**
   * `Free[S, ?]` has a monad for any type constructor `S[_]`.
   */
  implicit def freeMonad[S[_]]: Monad[Free[S, ?]] =
    new Monad[Free[S, ?]] {
      def pure[A](a: A): Free[S, A] = Free.pure(a)
      override def map[A, B](fa: Free[S, A])(f: A => B): Free[S, B] = fa.map(f)
      def flatMap[A, B](a: Free[S, A])(f: A => Free[S, B]): Free[S, B] = a.flatMap(f)
    }
}

import Free._

/**
 * A free operational monad for some functor `S`. Binding is done
 * using the heap instead of the stack, allowing tail-call
 * elimination.
 */
sealed abstract class Free[S[_], A] extends Product with Serializable {

  final def map[B](f: A => B): Free[S, B] =
    flatMap(a => Pure(f(a)))

  /**
   * Bind the given continuation to the result of this computation.
   * All left-associated binds are reassociated to the right.
   */
  final def flatMap[B](f: A => Free[S, B]): Free[S, B] =
    Gosub(this, f)

  /**
   * Catamorphism. Run the first given function if Pure, otherwise,
   * the second given function.
   */
  final def fold[B](r: A => B, s: S[Free[S, A]] => B)(implicit S: Functor[S]): B =
    resume.fold(s, r)

  /** Takes one evaluation step in the Free monad, re-associating left-nested binds in the process. */
  @tailrec
  final def step: Free[S, A] = this match {
    case Gosub(Gosub(c, f), g) => c.flatMap(cc => f(cc).flatMap(g)).step
    case Gosub(Pure(a), f) => f(a).step
    case x => x
  }

  /**
   * Evaluate a single layer of the free monad.
   */
  @tailrec
  final def resume(implicit S: Functor[S]): S[Free[S, A]] Xor A = this match {
    case Pure(a) => Right(a)
    case Suspend(t) => Left(S.map(t)(Pure(_)))
    case Gosub(c, f) =>
      c match {
        case Pure(a) => f(a).resume
        case Suspend(t) => Left(S.map(t)(f))
        case Gosub(d, g) => d.flatMap(dd => g(dd).flatMap(f)).resume
      }
  }

  /**
   * Run to completion, using a function that extracts the resumption
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

  final def run(implicit S: Comonad[S]): A = go(S.extract)

  /**
   * Run to completion, using a function that maps the resumption
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
   * Run to completion, mapping the suspension with the given transformation at each step and
   * accumulating into the monad `M`.
   */
  final def foldMap[M[_]](f: S ~> M)(implicit M: Monad[M]): M[A] =
    step match {
      case Pure(a) => M.pure(a)
      case Suspend(s) => f(s)
      case Gosub(c, g) => M.flatMap(c.foldMap(f))(cc => g(cc).foldMap(f))
    }

  /**
   * Compile your Free into another language by changing the suspension functor
   * using the given natural transformation.
   * Be careful if your natural transformation is effectful, effects are applied by mapSuspension.
   */
  final def mapSuspension[T[_]](f: S ~> T): Free[T, A] =
    foldMap[Free[T, ?]] {
      new NaturalTransformation[S, Free[T, ?]] {
        def apply[B](fa: S[B]): Free[T, B] = Suspend(f(fa))
      }
    }(Free.freeMonad)

  final def compile[T[_]](f: S ~> T): Free[T, A] = mapSuspension(f)

}

