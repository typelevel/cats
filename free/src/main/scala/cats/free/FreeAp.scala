package cats
package free

import cats.arrow.NaturalTransformation

object FreeAp {

  /**
   * Our case classes are the same as those provided by Free with the addition of Ap
   * which materializes the applicative ap behavior
   * This is useful when folding the FreeAp instance to a different Monad:
   * if that targeted monad has an `ap` function that differs from the usual flatMap / map sequence
   * we will retain that behavior
   */
  private final case class Pure[S[_], A](a: A) extends FreeAp[S, A]

  private final case class Suspend[S[_], A](a: S[A]) extends FreeAp[S, A]

  private final case class Gosub[S[_], B, C](c: FreeAp[S, C], f: C => FreeAp[S, B]) extends FreeAp[S, B]

  private final case class Ap[S[_], B, C](a: FreeAp[S, B], b: FreeAp[S, B => C]) extends FreeAp[S, C]

  def liftF[F[_], A](value: F[A]): FreeAp[F, A] = Suspend(value)

  def suspend[F[_], A](value: => FreeAp[F, A])(implicit F: Applicative[F]): FreeAp[F, A] =
    liftF(F.pure(())).flatMap(_ => value)

  def pure[S[_], A](a: A): FreeAp[S, A] = Pure(a)

  /**
   * `FreeAp[S, ?]` has a monad for any type constructor `S[_]`.
   */
  implicit def monad[S[_]]: Monad[FreeAp[S, ?]] =
    new Monad[FreeAp[S, ?]] {
      def pure[A](a: A): FreeAp[S, A] = FreeAp.pure(a)
      override def map[A, B](fa: FreeAp[S, A])(f: A => B): FreeAp[S, B] = fa.map(f)
      override def ap[A, B](ff: FreeAp[S, A => B])(fa: FreeAp[S, A]): FreeAp[S, B] = fa.ap(ff)
      override def product[A, B](fa: FreeAp[S, A], fb: FreeAp[S, B]): FreeAp[S, (A, B)] = ap(map(fa)(a => (b: B) => (a, b)))(fb)
      def flatMap[A, B](a: FreeAp[S, A])(f: A => FreeAp[S, B]): FreeAp[S, B] = a.flatMap(f)
    }
}

import FreeAp._

/**
 * A free monad that retains the `ap` structure
 */
sealed abstract class FreeAp[S[_], A] extends Product with Serializable {

  final def map[B](f: A => B): FreeAp[S, B] =
    ap(Pure(f))

  final def ap[B](b: FreeAp[S, A => B]): FreeAp[S, B] =
    Ap(this, b)

  final def flatMap[B](f: A => FreeAp[S, B]): FreeAp[S, B] =
    Gosub(this, f)

  final private def fold[M[_]](
    f: NaturalTransformation[Lambda[a => S[FreeAp[S, a]]], M]
  )(implicit S: Functor[S], M: Monad[M]): M[A] = this match {
    case Pure(a) => M.pure(a)
    case Suspend(t) => f(S.map(t)(Pure(_)))
    case Ap(a, b) => {
      val foldedLeft = a.fold(f)
      val foldedRight = b.fold(f)
      M.ap(foldedRight)(foldedLeft)
    }
    case Gosub(c, ff) => c match {
      case Pure(a) => ff(a).fold(f)
      case _ => M.flatMap(c.fold(f))(el => ff(el).fold(f))
    }
  }

  final def runM[M[_]](
    f: NaturalTransformation[Lambda[a => S[FreeAp[S, a]]], Lambda[a => M[FreeAp[S, a]]]]
  )(implicit S: Functor[S], M: Monad[M]): M[A] = {
    def interpret[B](apf: FreeAp[S, B]): M[B] = apf.fold(
      new NaturalTransformation[Lambda[a => S[FreeAp[S, a]]], M] {
        def apply[C](b: S[FreeAp[S, C]]): M[C] = {
          M.flatMap(f(b))(interpret)
        }
      })
    interpret(this)
  }

  final def foldMap[M[_]](f: NaturalTransformation[S, M])(implicit M: Monad[M]): M[A] =
    this match {
      case Pure(a) => M.pure(a)
      case Suspend(s) => f(s)
      case Gosub(c, g) => M.flatMap(c.foldMap(f))(cc => g(cc).foldMap(f))
      case Ap(a, b) => M.ap(b.foldMap(f))(a.foldMap(f))
    }

}
