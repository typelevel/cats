package cats
package free

import cats.arrow.NaturalTransformation

/** Applicative Functor for Free */
sealed abstract class FreeApplicative[F[_], A] extends Product with Serializable { self =>
  // ap => apply alias needed so we can refer to both
  // FreeApplicative.ap and FreeApplicative#ap
  import FreeApplicative.{FA, Pure, Ap, ap => apply, lift}

  final def ap[B](b: FA[F, A => B]): FA[F, B] =
    b match {
      case Pure(f) =>
        this.map(f)
      case x@Ap() =>
        apply(x.pivot)(self.ap(x.fn.map(fx => a => p => fx(p)(a))))
    }

  final def map[B](f: A => B): FA[F, B] =
    this match {
      case Pure(a) => Pure(f(a))
      case x@Ap() => apply(x.pivot)(x.fn.map(f compose _))
    }

  /** Natural Transformation of FreeApplicative based on given Natural Transformation */
  final def hoist[G[_]](f: F ~> G): FA[G, A] =
    this match {
      case Pure(a) => Pure[G, A](a)
      case x@Ap() => apply(f(x.pivot))(x.fn.hoist(f))
  }

  /** Interprets/Runs the sequence of operations using the semantics of Applicative G
    * Tail recursive only if G provides tail recursive interpretation (ie G is FreeMonad)
    */
  final def foldMap[G[_]](f: F ~> G)(implicit G: Applicative[G]): G[A] =
    this match {
      case Pure(a) => G.pure(a)
      case x@Ap() => G.ap(f(x.pivot))(x.fn.foldMap(f))
    }

  /** Interpret/run the operations using the semantics of `Applicative[F]`.
    * Tail recursive only if `F` provides tail recursive interpretation.
    */
  final def fold(implicit F: Applicative[F]): F[A] =
    foldMap(NaturalTransformation.id[F])

  final def compile[G[_]](f: F ~> G)(implicit G: Applicative[G]): FA[G, A] =
    foldMap[FA[G, ?]] {
      new NaturalTransformation[F, FA[G, ?]] {
        def apply[B](fa: F[B]): FA[G, B] = lift(f(fa))
      }
    }

  /** Compile this FreeApplicative algebra into a Free algebra. */
  final def monad: Free[F, A] =
    foldMap[Free[F, ?]] {
      new NaturalTransformation[F, Free[F, ?]] {
        def apply[B](fa: F[B]): Free[F, B] = Free.liftF(fa)
      }
    }
}

object FreeApplicative {
  type FA[F[_], A] = FreeApplicative[F, A]

  final case class Pure[F[_], A](a: A) extends FA[F, A]

  abstract case class Ap[F[_], A]() extends FA[F, A] {
    type Pivot
    val pivot: F[Pivot]
    val fn: FA[F, Pivot => A]
  }

  final def pure[F[_], A](a: A): FA[F, A] =
    Pure(a)

  final def ap[F[_], P, A](fp: F[P])(f: FA[F, P => A]): FA[F, A] =
    new Ap[F, A] {
      type Pivot = P
      val pivot: F[Pivot] = fp
      val fn: FA[F, Pivot => A] = f
    }

  final def lift[F[_], A](fa: F[A]): FA[F, A] =
    ap(fa)(Pure(a => a))

  implicit final def freeApplicative[S[_]]: Applicative[FA[S, ?]] = {
    new Applicative[FA[S, ?]] {
      def ap[A, B](fa: FA[S, A])(f: FA[S, A => B]): FA[S, B] = fa.ap(f)
      def pure[A](a: A): FA[S, A] = Pure(a)
    }
  }
}
