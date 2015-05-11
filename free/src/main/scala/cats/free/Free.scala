package cats
package free

import scala.annotation.tailrec

object Free {

  /**
   * Return from the computation with the given value.
   */
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

  /**
   * Suspend a value within a functor lifting it to a Free
   */
  def liftF[F[_], A](value: F[A])(implicit F: Functor[F]): Free[F, A] =
    Suspend(F.map(value)(Pure[F, A]))

  /**
   * Lift a value into the free functor and then suspend it in `Free`
   */
  def liftFC[F[_], A](value: F[A]): FreeC[F, A] =
    liftF[Coyoneda[F, ?], A](Coyoneda.lift(value))

  /**
   * Interpret a free monad over a free functor of `S` via natural
   * transformation to monad `M`.
   */
  def runFC[S[_], M[_], A](fa: FreeC[S, A])(f: S ~> M)(implicit M: Monad[M]): M[A] =
    fa.foldMap[M](new (Coyoneda[S, ?] ~> M) {
      def apply[B](ca: Coyoneda[S, B]): M[B] = M.map(f(ca.fi))(ca.k)
    })

  /**
   * `Free[S, ?]` has a monad if `S` has a `Functor`.
   */
  implicit def freeMonad[S[_]:Functor]: Monad[Free[S, ?]] =
    new Monad[Free[S, ?]] {
      def pure[A](a: A): Free[S, A] = Pure(a)
      override def map[A, B](fa: Free[S, A])(f: A => B): Free[S, B] = fa map f
      def flatMap[A, B](a: Free[S, A])(f: A => Free[S, B]): Free[S, B] = a flatMap f
    }
}

import Free._

/**
 * A free operational monad for some functor `S`. Binding is done
 * using the heap instead of the stack, allowing tail-call
 * elimination.
 */
sealed abstract class Free[S[_], A] extends Serializable {

  final def map[B](f: A => B): Free[S, B] =
    flatMap(a => Pure(f(a)))

  /**
   * Bind the given continuation to the result of this computation.
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
   * Evaluate a single layer of the free monad.
   */
  @tailrec
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

  def run(implicit S: Comonad[S]): A = go(S.extract)

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
  final def foldMap[M[_]](f: S ~> M)(implicit S: Functor[S], M: Monad[M]): M[A] =
    this.resume match {
      case Left(s) => Monad[M].flatMap(f(s))(_.foldMap(f))
      case Right(r) => Monad[M].pure(r)
    }

  /**
   * Compile your Free into another language by changing the suspension functor
   * using the given natural transformation.
   * Be careful if your natural transformation is effectful, effects are applied by mapSuspension.
   */
  final def mapSuspension[T[_]](f: S ~> T)(implicit S: Functor[S], T: Functor[T]): Free[T, A] =
    resume match {
      case Left(s)  => Suspend(f(S.map(s)(((_: Free[S, A]) mapSuspension f))))
      case Right(r) => Pure(r)
    }

  final def compile[T[_]](f: S ~> T)(implicit S: Functor[S], T: Functor[T]): Free[T, A] = mapSuspension(f)

}

