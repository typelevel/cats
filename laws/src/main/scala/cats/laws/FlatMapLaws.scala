package cats
package laws

import cats.data.Kleisli
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._

/**
 * Laws that must be obeyed by any `FlatMap`.
 */
trait FlatMapLaws[F[_]] extends ApplyLaws[F] {
  implicit override def F: FlatMap[F]

  def flatMapAssociativity[A, B, C](fa: F[A], f: A => F[B], g: B => F[C]): IsEq[F[C]] =
    fa.flatMap(f).flatMap(g) <-> fa.flatMap(a => f(a).flatMap(g))

  def flatMapConsistentApply[A, B](fa: F[A], fab: F[A => B]): IsEq[F[B]] =
    fab.ap(fa) <-> fab.flatMap(f => fa.map(f))

  def followedByConsistency[A, B](fa: F[A], fb: F[B]): IsEq[F[B]] =
    F.followedBy(fa)(fb) <-> F.flatMap(fa)(_ => fb)

  /**
   * The composition of `cats.data.Kleisli` arrows is associative. This is
   * analogous to [[flatMapAssociativity]].
   */
  def kleisliAssociativity[A, B, C, D](f: A => F[B], g: B => F[C], h: C => F[D], a: A): IsEq[F[D]] = {
    val (kf, kg, kh) = (Kleisli(f), Kleisli(g), Kleisli(h))
    ((kf andThen kg) andThen kh).run(a) <-> (kf andThen (kg andThen kh)).run(a)
  }

  def mproductConsistency[A, B](fa: F[A], fb: A => F[B]): IsEq[F[(A, B)]] =
    F.mproduct(fa)(fb) <-> F.flatMap(fa)(a => F.map(fb(a))((a, _)))

  def tailRecMConsistentFlatMap[A](count: Int, a: A, f: A => F[A]): IsEq[F[A]] = {
    def bounce(n: Int) = F.tailRecM[(A, Int), A]((a, n)) { case (a0, i) =>
      if (i > 0) f(a0).map(a1 => Left((a1, i-1)))
      else f(a0).map(Right(_))
    }
    /*
     * The law is for n >= 1
     * bounce(n) == bounce(n - 1).flatMap(f)
     * many monads blow up if n gets too large here
     * (for instance List, becomes multiplicative, so
     * the memory is exponential in n).
     */
    val smallN = (count % 2) + 2 // a number 1 to 3
    bounce(smallN) <-> bounce(smallN - 1).flatMap(f)
  }
}

object FlatMapLaws {
  def apply[F[_]](implicit ev: FlatMap[F]): FlatMapLaws[F] =
    new FlatMapLaws[F] { def F: FlatMap[F] = ev }
}
