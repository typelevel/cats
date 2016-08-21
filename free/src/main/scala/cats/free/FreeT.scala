package cats
package free

import scala.annotation.tailrec

/**
 * FreeT is a monad transformer for Free monads over a Functor S
 *
 * Stack safety for `Free` and `FreeT` is based on the paper
 * [[http://functorial.com/stack-safety-for-free/index.pdf Stack Safety for Free]] by Phil Freeman
 *
 * This Scala implementation of `FreeT` and its usages are derived from
 * [[https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/FreeT.scala Scalaz's FreeT]],
 * originally written by Brian McKenna.
 */
sealed abstract class FreeT[S[_], M[_], A] extends Product with Serializable {
  import FreeT._

  final def map[B](f: A => B)(implicit M: Applicative[M]): FreeT[S, M, B] =
    flatMap(a => pure(f(a)))

  /** Binds the given continuation to the result of this computation. */
  final def flatMap[B](f: A => FreeT[S, M, B]): FreeT[S, M, B] =
    FlatMapped(this, f)

  /**
   * Changes the underlying `Monad` for this `FreeT`, ie.
   * turning this `FreeT[S, M, A]` into a `FreeT[S, N, A]`.
   */
  def hoist[N[_]](mn: M ~> N): FreeT[S, N, A] =
    step match {
      case e @ FlatMapped(_, _) =>
        FlatMapped(e.a.hoist(mn), e.f.andThen(_.hoist(mn)))
      case Suspend(m) =>
        Suspend(mn(m))
    }

  /** Change the base functor `S` for a `FreeT` action. */
  def interpret[T[_]](st: S ~> T)(implicit M: Functor[M]): FreeT[T, M, A] =
    step match {
      case e @ FlatMapped(_, _) =>
        FlatMapped(e.a.interpret(st), e.f.andThen(_.interpret(st)))
      case Suspend(m) =>
        Suspend(M.map(m)(_.left.map(s => st(s))))
    }

  /**
   * Runs to completion, mapping the suspension with the given transformation
   * at each step and accumulating into the monad `M`.
   */
  def foldMap(f: S ~> M)(implicit MR: Monad[M], RT: RecursiveTailRecM[M]): M[A] = {
    def go(ft: FreeT[S, M, A]): M[Either[FreeT[S, M, A], A]] =
      ft match {
        case Suspend(ma) => MR.flatMap(ma) {
          case Right(a) => MR.pure(Right(a))
          case Left(sa) => MR.map(f(sa))(Right(_))
        }
        case g @ FlatMapped(_, _) => g.a match {
          case Suspend(mx) => MR.flatMap(mx) {
            case Right(x) => MR.pure(Left(g.f(x)))
            case Left(sx) => MR.map(f(sx))(x => Left(g.f(x)))
          }
          case g0 @ FlatMapped(_, _) => MR.pure(Left(g0.a.flatMap(g0.f(_).flatMap(g.f))))
        }
      }

    RT.sameType(MR).tailRecM(this)(go)
  }

  /** Evaluates a single layer of the free monad */
  def resume(implicit S: Functor[S], MR: Monad[M], RT: RecursiveTailRecM[M]): M[Either[S[FreeT[S, M, A]], A]] = {
    def go(ft: FreeT[S, M, A]): M[Either[FreeT[S, M, A], Either[S[FreeT[S, M, A]], A]]] =
      ft match {
        case Suspend(f) => MR.map(f)(as => Right(as.left.map(S.map(_)(pure(_)))))
        case g1 @ FlatMapped(_, _) => g1.a match {
          case Suspend(m1) => MR.map(m1) {
            case Right(a) => Left(g1.f(a))
            case Left(fc) => Right(Left(S.map(fc)(g1.f(_))))
          }
          case g2 @ FlatMapped(_, _) => MR.pure(Left(g2.a.flatMap(g2.f(_).flatMap(g1.f))))
        }
      }

    RT.sameType(MR).tailRecM(this)(go)
  }

  /**
   * Runs to completion, using a function that maps the resumption from `S` to a monad `M`.
   */
  def runM(interp: S[FreeT[S, M, A]] => M[FreeT[S, M, A]])(implicit S: Functor[S], MR: Monad[M], RT: RecursiveTailRecM[M]): M[A] = {
    def runM2(ft: FreeT[S, M, A]): M[Either[FreeT[S, M, A], A]] =
      MR.flatMap(ft.resume) {
        case Right(a) => MR.pure(Right(a))
        case Left(fc) => MR.map(interp(fc))(Left(_))
      }
    RT.sameType(MR).tailRecM(this)(runM2)
  }

  /**
   * Finds the first `M` instance, `m`, and maps it to contain the rest
   * of the computation. Since only `map` is used on `m`, its structure
   * is preserved.
   */
  @tailrec
  private[cats] final def toM(implicit M: Applicative[M]): M[FreeT[S, M, A]] =
    this match {
      case Suspend(m) => M.map(m) {
        case Right(a) => pure(a)
        case Left(s) => liftF(s)
      }
      case g1 @ FlatMapped(_, _) => g1.a match {
        case Suspend(m) => M.map(m) {
          case Right(a) => g1.f(a)
          case Left(s) => liftF[S, M, g1.A](s).flatMap(g1.f)
        }
        case g0 @ FlatMapped(_, _) => g0.a.flatMap(g0.f(_).flatMap(g1.f)).toM
      }
    }

  @tailrec
  private def step: FreeT[S, M, A] =
    this match {
      case g @ FlatMapped(_, _) => g.a match {
        case g0 @ FlatMapped(_, _) => g0.a.flatMap(a => g0.f(a).flatMap(g.f)).step
        case _ => g
      }
      case x => x
    }

  override def toString(): String = "FreeT(...)"
}

object FreeT extends FreeTInstances {
  /** Suspend the computation with the given suspension. */
  private[free] case class Suspend[S[_], M[_], A](a: M[Either[S[A], A]]) extends FreeT[S, M, A]

  /** Call a subroutine and continue with the given function. */
  private[free] case class FlatMapped[S[_], M[_], A0, B](a0: FreeT[S, M, A0], f0: A0 => FreeT[S, M, B]) extends FreeT[S, M, B] {
    type A = A0
    def a: FreeT[S, M, A] = a0
    def f: A => FreeT[S, M, B] = f0
  }

  /** Return the given value in the free monad. */
  def pure[S[_], M[_], A](value: A)(implicit M: Applicative[M]): FreeT[S, M, A] = Suspend(M.pure(Right(value)))

  def suspend[S[_], M[_], A](a: M[Either[A, S[FreeT[S, M, A]]]])(implicit M: Applicative[M]): FreeT[S, M, A] =
    liftT(a).flatMap({
      case Left(a) => pure(a)
      case Right(s) => roll(s)
    })

  def tailRecM[S[_], M[_]: Applicative, A, B](a: A)(f: A => FreeT[S, M, Either[A, B]]): FreeT[S, M, B] =
    f(a).flatMap {
      case Left(a0) => tailRecM(a0)(f)
      case Right(b) => pure[S, M, B](b)
    }

  def liftT[S[_], M[_], A](value: M[A])(implicit M: Functor[M]): FreeT[S, M, A] =
    Suspend(M.map(value)(Right(_)))

  /** A version of `liftT` that infers the nested type constructor. */
  def liftTU[S[_], MA](value: MA)(implicit M: Unapply[Functor, MA]): FreeT[S, M.M, M.A] =
    liftT[S, M.M, M.A](M.subst(value))(M.TC)

  /** Suspends a value within a functor in a single step. Monadic unit for a higher-order monad. */
  def liftF[S[_], M[_], A](value: S[A])(implicit M: Applicative[M]): FreeT[S, M, A] =
    Suspend(M.pure(Left(value)))

  def roll[S[_], M[_], A](value: S[FreeT[S, M, A]])(implicit M: Applicative[M]): FreeT[S, M, A] =
    liftF[S, M, FreeT[S, M, A]](value).flatMap(identity)

}

private[free] sealed trait FreeTInstances3 {
  implicit def catsFreeMonadStateForFreeT[S[_], M[_], E](implicit M1: MonadState[M, E]): MonadState[FreeT[S, M, ?], E] =
    new MonadState[FreeT[S, M, ?], E] with FreeTMonad[S, M] {
      override def M = implicitly
      override def get =
        FreeT.liftT(M1.get)
      override def set(s: E) =
        FreeT.liftT(M1.set(s))
    }
}

private[free] sealed trait FreeTInstances2 extends FreeTInstances3 {
  implicit def catsFreeMonadErrorForFreeT[S[_], M[_]: RecursiveTailRecM, E](implicit E: MonadError[M, E]): MonadError[FreeT[S, M, ?], E] =
    new MonadError[FreeT[S, M, ?], E] with FreeTMonad[S, M] {
      override def M = implicitly
      override def handleErrorWith[A](fa: FreeT[S, M, A])(f: E => FreeT[S, M, A]) =
        FreeT.liftT[S, M, FreeT[S, M, A]](E.handleErrorWith(fa.toM)(f.andThen(_.toM)))(M).flatMap(identity)
      override def raiseError[A](e: E) =
        FreeT.liftT(E.raiseError[A](e))(M)
    }
}

private[free] sealed trait FreeTInstances1 extends FreeTInstances2 {
  implicit def catsFreeFlatMapForFreeT[S[_], M[_]](implicit M0: Applicative[M]): FlatMap[FreeT[S, M, ?]] =
    new FreeTFlatMap[S, M] {
      implicit def M: Applicative[M] = M0
    }

  implicit def catsFreeTransLiftForFreeT[S[_]]: TransLift.Aux[FreeT[S, ?[_], ?], Functor] =
    new TransLift[FreeT[S, ?[_], ?]] {

      type TC[M[_]] = Functor[M]

      override def liftT[M[_]: Functor, A](ma: M[A]): FreeT[S, M, A] =
        FreeT.liftT(ma)
    }
}

private[free] sealed trait FreeTInstances0 extends FreeTInstances1 {
  implicit def catsFreeMonadForFreeT[S[_], M[_]](implicit M0: Applicative[M]): Monad[FreeT[S, M, ?]] with RecursiveTailRecM[FreeT[S, M, ?]] =
    new FreeTMonad[S, M] {
      def M = M0
    }

  implicit def catsFreeCombineForFreeT[S[_], M[_]: Applicative: SemigroupK]: SemigroupK[FreeT[S, M, ?]] =
    new FreeTCombine[S, M] {
      override def M = implicitly
      override def M1 = implicitly
    }
}

private[free] sealed trait FreeTInstances extends FreeTInstances0 {
  implicit def catsFreeMonadCombineForFreeT[S[_], M[_]: Alternative]: MonadCombine[FreeT[S, M, ?]] =
    new MonadCombine[FreeT[S, M, ?]] with FreeTCombine[S, M] with FreeTMonad[S, M] {
      override def M = implicitly
      override def M1 = implicitly

      override def empty[A] = FreeT.liftT[S, M, A](MonoidK[M].empty[A])(M)
    }
}

private[free] sealed trait FreeTFlatMap[S[_], M[_]] extends FlatMap[FreeT[S, M, ?]] {
  implicit def M: Applicative[M]

  override final def map[A, B](fa: FreeT[S, M, A])(f: A => B): FreeT[S, M, B] = fa.map(f)
  def flatMap[A, B](fa: FreeT[S, M, A])(f: A => FreeT[S, M, B]): FreeT[S, M, B] = fa.flatMap(f)
  override final def tailRecM[A, B](a: A)(f: A => FreeT[S, M, Either[A, B]]): FreeT[S, M, B] =
    FreeT.tailRecM(a)(f)
}

private[free] sealed trait FreeTMonad[S[_], M[_]] extends Monad[FreeT[S, M, ?]] with RecursiveTailRecM[FreeT[S, M, ?]] with FreeTFlatMap[S, M] {
  implicit def M: Applicative[M]

  override final def pure[A](a: A): FreeT[S, M, A] =
    FreeT.pure[S, M, A](a)
}

private[free] sealed trait FreeTCombine[S[_], M[_]] extends SemigroupK[FreeT[S, M, ?]] {
  implicit def M: Applicative[M]
  def M1: SemigroupK[M]
  override final def combineK[A](a: FreeT[S, M, A], b: FreeT[S, M, A]): FreeT[S, M, A] =
    FreeT.liftT(M1.combineK(a.toM, b.toM))(M).flatMap(identity)
}
