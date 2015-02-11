package cats
package free

/** Applicative Functor for Free */
sealed abstract class FreeApplicative[F[_], A] {
  self =>

  import FreeApplicative._

  final def ap[B](b: FreeApplicative[F, A => B]): FreeApplicative[F, B] = b match {

    case Pure(f) => this map f

    case x@Ap() => new Ap[F, B] {
      type Pivot = x.Pivot
      val pivot = x.pivot
      val fn: FreeApplicative[F, x.Pivot => B] = self.ap(x.fn map { fx: (x.Pivot => A => B) => (a:A) => (p: x.Pivot) => fx(p)(a) })
    }
  }

  final def map[B](f: A => B): FreeApplicative[F, B] = this match {

    case Pure(a) => Pure(f(a))

    case x@Ap() => new Ap[F, B] {
      type Pivot = x.Pivot
      val pivot = x.pivot
      val fn = x.fn map (f compose _)
    }
  }

  /** Natural Transformation of FreeApplicative based on given Natural Transformation */
  final def hoist[G[_]](f: F ~> G): FreeApplicative[G, A] = this match {

    case Pure(a) => Pure[G, A](a)

    case x@Ap() => new Ap[G, A] {
      type Pivot = x.Pivot
      val pivot = f(x.pivot)
      val fn = x.fn.hoist(f)
    }
  }

  /** Interpretes/Runs the sequence of operations using the semantics of Applicative G
    * Tail recursive only if G provides tail recursive interpretation (ie G is FreeMonad)
    */
  final def run[G[_]](f: F ~> G)(implicit G: Applicative[G]): G[A] = this match {

    case Pure(a) => G.pure(a)

    case x@Ap() => G.apply(f(x.pivot))(x.fn.run(f))
  }


}

object FreeApplicative {

  def pure[F[_], A](a: A) = Pure[F, A](a)

  def lift[F[_], A](fa: F[A]) = new Ap[F, A] {
    type Pivot = A
    val pivot = fa
    val fn = pure[F, A => A]((a:A) => a)
  }

  case class Pure[F[_], A](a: A) extends FreeApplicative[F, A]

  abstract case class Ap[F[_], A]() extends FreeApplicative[F, A] {
    type Pivot

    val pivot: F[Pivot]
    val fn: FreeApplicative[F, Pivot => A]
  }

  implicit def theFreeApplicative[S[_]] = new Applicative[FreeApplicative[S, ?]] {

    def apply[A, B](fa: FreeApplicative[S, A])(f: FreeApplicative[S, A => B]): FreeApplicative[S, B] = fa.ap(f)

    def pure[A](a: A): FreeApplicative[S, A] = FreeApplicative.pure(a)

  }
}
