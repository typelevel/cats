package cats
package free

import scala.annotation.tailrec

import cats.arrow.FunctionK

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

  /**
    * Modify the context `M` using transformation `mn`.
    */
  def mapK[N[_]](mn: M ~> N): FreeT[S, N, A] =
    step match {
      case e @ FlatMapped(_, _) =>
        FlatMapped(e.a.mapK(mn), e.f.andThen(_.mapK(mn)))
      case Suspend(m) =>
        Suspend(mn(m))
    }

  /** Binds the given continuation to the result of this computation. */
  final def flatMap[B](f: A => FreeT[S, M, B]): FreeT[S, M, B] =
    FlatMapped(this, f)

  /**
   * Changes the underlying `Monad` for this `FreeT`, ie.
   * turning this `FreeT[S, M, A]` into a `FreeT[S, N, A]`.
    */
  def hoist[N[_]](mn: FunctionK[M, N]): FreeT[S, N, A] =
    mapK(mn)

  @deprecated("Use compile", "0.8.0")
  def interpret[T[_]](st: FunctionK[S, T])(implicit M: Functor[M]): FreeT[T, M, A] = compile(st)

  /** Change the base functor `S` for a `FreeT` action. */
  def compile[T[_]](st: FunctionK[S, T])(implicit M: Functor[M]): FreeT[T, M, A] =
    step match {
      case e @ FlatMapped(_, _) =>
        FlatMapped(e.a.compile(st), e.f.andThen(_.compile(st)))
      case Suspend(m) =>
        Suspend(M.map(m)(_.left.map(s => st(s))))
    }

  /**
   * Runs to completion, mapping the suspension with the given transformation
   * at each step and accumulating into the monad `M`.
   */
  def foldMap(f: FunctionK[S, M])(implicit M: Monad[M]): M[A] = {
    def go(ft: FreeT[S, M, A]): M[Either[FreeT[S, M, A], A]] =
      ft match {
        case Suspend(ma) => M.flatMap(ma) {
          case Right(a) => M.pure(Right(a))
          case Left(sa) => M.map(f(sa))(Right(_))
        }
        case g @ FlatMapped(_, _) => g.a match {
          case Suspend(mx) => M.flatMap(mx) {
            case Right(x) => M.pure(Left(g.f(x)))
            case Left(sx) => M.map(f(sx))(x => Left(g.f(x)))
          }
          case g0 @ FlatMapped(_, _) => M.pure(Left(g0.a.flatMap(g0.f(_).flatMap(g.f))))
        }
      }

    M.tailRecM(this)(go)
  }

  /** Evaluates a single layer of the free monad */
  def resume(implicit S: Functor[S], M: Monad[M]): M[Either[S[FreeT[S, M, A]], A]] = {
    def go(ft: FreeT[S, M, A]): M[Either[FreeT[S, M, A], Either[S[FreeT[S, M, A]], A]]] =
      ft match {
        case Suspend(f) => M.map(f)(as => Right(as.left.map(S.map(_)(pure(_)))))
        case g1 @ FlatMapped(_, _) => g1.a match {
          case Suspend(m1) => M.map(m1) {
            case Right(a) => Left(g1.f(a))
            case Left(fc) => Right(Left(S.map(fc)(g1.f(_))))
          }
          case g2 @ FlatMapped(_, _) => M.pure(Left(g2.a.flatMap(g2.f(_).flatMap(g1.f))))
        }
      }

    M.tailRecM(this)(go)
  }

  /**
   * Runs to completion, using a function that maps the resumption from `S` to a monad `M`.
   */
  def runM(interp: S[FreeT[S, M, A]] => M[FreeT[S, M, A]])(implicit S: Functor[S], M: Monad[M]): M[A] = {
    def runM2(ft: FreeT[S, M, A]): M[Either[FreeT[S, M, A], A]] =
      M.flatMap(ft.resume) {
        case Right(a) => M.pure(Right(a))
        case Left(fc) => M.map(interp(fc))(Left(_))
      }
    M.tailRecM(this)(runM2)
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

  override def toString: String = "FreeT(...)"
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

  @deprecated("Use FreeT.defer.", "1.0.0-MF")
  def suspend[S[_], M[_], A](a: M[Either[A, S[FreeT[S, M, A]]]])(implicit M: Applicative[M]): FreeT[S, M, A] =
    defer(a)

  def defer[S[_], M[_], A](a: M[Either[A, S[FreeT[S, M, A]]]])(implicit M: Applicative[M]): FreeT[S, M, A] =
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

  /** Suspends a value within a functor in a single step. Monadic unit for a higher-order monad. */
  def liftF[S[_], M[_], A](value: S[A])(implicit M: Applicative[M]): FreeT[S, M, A] =
    Suspend(M.pure(Left(value)))

  def roll[S[_], M[_], A](value: S[FreeT[S, M, A]])(implicit M: Applicative[M]): FreeT[S, M, A] =
    liftF[S, M, FreeT[S, M, A]](value).flatMap(identity)

  def compile[S[_], T[_], M[_]: Functor](st: FunctionK[S, T]): FunctionK[FreeT[S, M, ?], FreeT[T, M, ?]] =
    λ[FunctionK[FreeT[S, M, ?], FreeT[T, M, ?]]](f => f.compile(st))

  def foldMap[S[_], M[_]: Monad](fk: FunctionK[S, M]): FunctionK[FreeT[S, M, ?], M] =
    λ[FunctionK[FreeT[S, M, ?], M]](f => f.foldMap(fk))
}

private[free] sealed abstract class FreeTInstances extends FreeTInstances0 {
  implicit def catsFreeMonadErrorForFreeT[S[_], M[_], E](implicit E: MonadError[M, E]): MonadError[FreeT[S, M, ?], E] =
    new MonadError[FreeT[S, M, ?], E] with FreeTMonad[S, M] {
      override def M = E
      override def handleErrorWith[A](fa: FreeT[S, M, A])(f: E => FreeT[S, M, A]) =
        FreeT.liftT[S, M, FreeT[S, M, A]](E.handleErrorWith(fa.toM)(f.andThen(_.toM)))(M).flatMap(identity)
      override def raiseError[A](e: E) =
        FreeT.liftT(E.raiseError[A](e))(M)
    }
}

private[free] sealed abstract class FreeTInstances0 extends FreeTInstances1 {
  implicit def catsFreeMonadForFreeT[S[_], M[_]](implicit M0: Applicative[M]): Monad[FreeT[S, M, ?]] =
    new FreeTMonad[S, M] {
      def M = M0
    }
}

private[free] sealed abstract class FreeTInstances1 extends FreeTInstances2 {
  implicit def catsFreeFlatMapForFreeT[S[_], M[_]](implicit M0: Applicative[M]): FlatMap[FreeT[S, M, ?]] =
    new FreeTFlatMap[S, M] {
      implicit def M: Applicative[M] = M0
    }
}

private[free] sealed abstract class FreeTInstances2 extends FreeTInstances3 {
  implicit def catsFreeAlternativeForFreeT[S[_], M[_]: Alternative: Monad]: Alternative[FreeT[S, M, ?]] =
    new Alternative[FreeT[S, M, ?]] with FreeTMonad[S, M] with FreeTMonoidK[S, M] {
      override def M = Alternative[M]
      override def M1 = Alternative[M]
    }
}

private[free] sealed abstract class FreeTInstances3 {
  implicit def catsFreeSemigroupKForFreeT[S[_], M[_]: Applicative: SemigroupK]: SemigroupK[FreeT[S, M, ?]] =
    new FreeTSemigroupK[S, M] {
      override def M = Applicative[M]
      override def M1 = SemigroupK[M]
    }
}

private[free] sealed trait FreeTFlatMap[S[_], M[_]] extends FlatMap[FreeT[S, M, ?]] {
  implicit def M: Applicative[M]

  override final def map[A, B](fa: FreeT[S, M, A])(f: A => B): FreeT[S, M, B] = fa.map(f)
  def flatMap[A, B](fa: FreeT[S, M, A])(f: A => FreeT[S, M, B]): FreeT[S, M, B] = fa.flatMap(f)
  override final def tailRecM[A, B](a: A)(f: A => FreeT[S, M, Either[A, B]]): FreeT[S, M, B] =
    FreeT.tailRecM(a)(f)
}

private[free] sealed trait FreeTMonad[S[_], M[_]] extends Monad[FreeT[S, M, ?]] with FreeTFlatMap[S, M] {
  implicit def M: Applicative[M]

  override final def pure[A](a: A): FreeT[S, M, A] =
    FreeT.pure[S, M, A](a)
}

private[free] sealed trait FreeTMonoidK[S[_], M[_]] extends MonoidK[FreeT[S, M, ?]] with FreeTSemigroupK[S, M] {
  implicit def M: Applicative[M]
  def M1: MonoidK[M]
  override final def empty[A]: FreeT[S, M, A] = FreeT.liftT[S, M, A](M1.empty[A])(M)
}

private[free] sealed trait FreeTSemigroupK[S[_], M[_]] extends SemigroupK[FreeT[S, M, ?]] {
  implicit def M: Applicative[M]
  def M1: SemigroupK[M]
  override final def combineK[A](a: FreeT[S, M, A], b: FreeT[S, M, A]): FreeT[S, M, A] =
    FreeT.liftT(M1.combineK(a.toM, b.toM))(M).flatMap(identity)
}
