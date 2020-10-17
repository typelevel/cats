package cats.free

import Free.{FlatMapped, Pure, Suspend}
import cats.{Foldable, Eval}

private[free] trait FreeFoldStep[S[_], A] {

  def step: Free[S, A]

  private type OnFlatMapped[X] = (S[X], X => Free[S, A])

  /**
   * A combination of step and fold. May be used to define interpreters with custom
   * (non-monoidial) control flow.
   */
  final def foldStep[B](
    onPure: A => B,
    onSuspend: S[A] => B,
    onFlatMapped: [X] => (S[X], X => Free[S, A]) => B
  ): B =
    this.step match {
      case Pure(a)                    => onPure(a)
      case Suspend(a)                 => onSuspend(a)
      case FlatMapped(Suspend(fa), f) => onFlatMapped(fa, f)
      case _                          => sys.error("FlatMapped should be right associative after step")
    }

  final def foldLeft[B](fa: Free[S, A], b: B)(f: (B, A) => B)(implicit F: Foldable[S]): B =
    fa.foldStep(
      a => f(b, a),
      fa => F.foldLeft(fa, b)(f),
      [X] => (sx: S[X], g: X => Free[S, A]) => F.foldLeft(sx, b)((bb, x) => foldLeft(g(x), bb)(f))
    )

  final def foldRight[B](fa: Free[S, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[S]): Eval[B] =
    fa.foldStep(
      a => f(a, lb),
      fa => F.foldRight(fa, lb)(f),
      [X] => (sx: S[X], g: X => Free[S, A]) => F.foldRight(sx, lb)((a, lbb) => foldRight(g(a), lbb)(f))
    )
}