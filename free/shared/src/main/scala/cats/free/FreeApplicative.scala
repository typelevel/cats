package cats
package free

/** Applicative Functor for Free */
sealed abstract class FreeApplicative[F[_], A] { self =>

  import FreeApplicative.{FA, Pure, Ap}

  final def ap[B](b: FA[F, A => B]): FA[F, B] =
    b match {
      case Pure(f) =>
        this.map(f)
      case x: Ap[F, A => B] =>
        Ap(x.pivot)(self.ap(x.fn.map(fx => a => p => fx(p)(a))))
    }

  final def map[B](f: A => B): FA[F, B] =
    this match {
      case Pure(a) => Pure(f(a))
      case x: Ap[F, A] => Ap(x.pivot)(x.fn.map(f compose _))
    }

  /** Natural Transformation of FreeApplicative based on given Natural Transformation */
  final def hoist[G[_]](f: F ~> G): FA[G, A] =
    this match {
      case Pure(a) => Pure[G, A](a)
      case x: Ap[F, A] => Ap(f(x.pivot))(x.fn.hoist(f))
  }

  /** Interpretes/Runs the sequence of operations using the semantics of Applicative G
    * Tail recursive only if G provides tail recursive interpretation (ie G is FreeMonad)
    */
  final def run[G[_]](f: F ~> G)(implicit G: Applicative[G]): G[A] =
    this match {
      case Pure(a) => G.pure(a)
      case x: Ap[F, A] => G.ap(f(x.pivot))(x.fn.run(f))
    }
}

object FreeApplicative {

  type FA[F[_], A] = FreeApplicative[F, A]

  final case class Pure[F[_], A](a: A) extends FA[F, A]

  abstract class Ap[F[_], A] extends FA[F, A] {
    type Pivot
    val pivot: F[Pivot]
    val fn: FA[F, Pivot => A]
  }

  final def pure[F[_], A](a: A): FA[F, A] =
    Pure(a)

  final def Ap[F[_], P, A](fp: F[P])(f: FA[F, P => A]): FA[F, A] =
    new Ap[F, A] {
      type Pivot = P
      val pivot: F[Pivot] = fp
      val fn: FA[F, Pivot => A] = f
    }

  final def lift[F[_], A](fa: F[A]): FA[F, A] =
    Ap(fa)(Pure(a => a))

  implicit final def freeApplicative[S[_]]: Applicative[FA[S, ?]] = {
    new Applicative[FA[S, ?]] {
      def ap[A, B](fa: FA[S, A])(f: FA[S, A => B]): FA[S, B] = fa.ap(f)
      def pure[A](a: A): FA[S, A] = Pure(a)
    }
  }
}
