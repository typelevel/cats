package cats
package free

import cats.arrow.FunctionK
import cats.data.Const

/** Applicative Functor for Free */
sealed abstract class FreeApplicative[F[_], A] extends Product with Serializable { self =>
  // ap => apply alias needed so we can refer to both
  // FreeApplicative.ap and FreeApplicative#ap
  import FreeApplicative.{FA, Pure, Ap, ap => apply, lift}

  final def ap[B](b: FA[F, A => B]): FA[F, B] =
    b match {
      case Pure(f) =>
        this.map(f)
      case Ap(pivot, fn) =>
        apply(pivot)(self.ap(fn.map(fx => a => p => fx(p)(a))))
    }

  final def map[B](f: A => B): FA[F, B] =
    this match {
      case Pure(a) => Pure(f(a))
      case Ap(pivot, fn) => apply(pivot)(fn.map(f compose _))
    }

  /** Interprets/Runs the sequence of operations using the semantics of Applicative G
   * Tail recursive only if G provides tail recursive interpretation (ie G is FreeMonad)
   */
  final def foldMap[G[_]](f: FunctionK[F, G])(implicit G: Applicative[G]): G[A] =
    this match {
      case Pure(a) => G.pure(a)
      case Ap(pivot, fn) => G.map2(f(pivot), fn.foldMap(f))((a, g) => g(a))
    }

  /** Interpret/run the operations using the semantics of `Applicative[F]`.
   * Tail recursive only if `F` provides tail recursive interpretation.
   */
  final def fold(implicit F: Applicative[F]): F[A] =
    foldMap(FunctionK.id[F])

  /** Interpret this algebra into another FreeApplicative */
  final def compile[G[_]](f: FunctionK[F, G]): FA[G, A] =
    foldMap[FA[G, ?]] {
      new FunctionK[F, FA[G, ?]] {
        def apply[B](fa: F[B]): FA[G, B] = lift(f(fa))
      }
    }

  /** Interpret this algebra into a Monoid */
  final def analyze[M:Monoid](f: FunctionK[F, λ[α => M]]): M =
    foldMap[Const[M, ?]](new (FunctionK[F, Const[M, ?]]) {
      def apply[X](x: F[X]): Const[M, X] = Const(f(x))
    }).getConst

  /** Compile this FreeApplicative algebra into a Free algebra. */
  final def monad: Free[F, A] =
    foldMap[Free[F, ?]] {
      new FunctionK[F, Free[F, ?]] {
        def apply[B](fa: F[B]): Free[F, B] = Free.liftF(fa)
      }
    }

  override def toString: String = "FreeApplicative(...)"
}

object FreeApplicative {
  type FA[F[_], A] = FreeApplicative[F, A]

  private final case class Pure[F[_], A](a: A) extends FA[F, A]

  private final case class Ap[F[_], P, A](pivot: F[P], fn: FA[F, P => A]) extends FA[F, A]

  final def pure[F[_], A](a: A): FA[F, A] =
    Pure(a)

  final def ap[F[_], P, A](fp: F[P])(f: FA[F, P => A]): FA[F, A] = Ap(fp, f)

  final def lift[F[_], A](fa: F[A]): FA[F, A] =
    ap(fa)(Pure(a => a))

  implicit final def freeApplicative[S[_]]: Applicative[FA[S, ?]] = {
    new Applicative[FA[S, ?]] {
      override def product[A, B](fa: FA[S, A], fb: FA[S, B]): FA[S, (A, B)] = ap(fa.map((a: A) => (b: B) => (a, b)))(fb)
      override def map[A, B](fa: FA[S, A])(f: A => B): FA[S, B] = fa.map(f)
      override def ap[A, B](f: FA[S, A => B])(fa: FA[S, A]): FA[S, B] = fa.ap(f)
      def pure[A](a: A): FA[S, A] = Pure(a)
    }
  }
}
