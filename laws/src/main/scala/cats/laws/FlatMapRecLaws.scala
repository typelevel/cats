package cats
package laws

import cats.syntax.flatMap._
import cats.syntax.functor._

/**
 * Laws that must be obeyed by any `FlatMapRec`.
 */
trait FlatMapRecLaws[F[_]] extends FlatMapLaws[F] {
  implicit override def F: FlatMapRec[F]

  def tailRecMConsistentFlatMap[A](a: A, f: A => F[A]): IsEq[F[A]] = {
    val bounce = F.tailRecM[(A, Int), A]((a, 1)) { case (a0, i) =>
      if (i > 0) f(a0).map(a1 => Left((a1, i-1)))
      else f(a0).map(Right(_))
    }
    bounce <-> f(a).flatMap(f)
  }
}

object FlatMapRecLaws {
  def apply[F[_]](implicit ev: FlatMapRec[F]): FlatMapRecLaws[F] =
    new FlatMapRecLaws[F] { def F: FlatMapRec[F] = ev }
}
