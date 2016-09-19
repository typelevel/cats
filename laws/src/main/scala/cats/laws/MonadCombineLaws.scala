package cats
package laws

// import cats.syntax.all._

/**
 * Laws that must be obeyed by any `MonadCombine`.
 */
trait MonadCombineLaws[F[_]] extends MonadFilterLaws[F] with AlternativeLaws[F] {
  implicit override def F: MonadCombine[F]

  // the left distributivity law does not hold for things like
  // MonadCombine[Option]; here's a counter-example:
  //
  //     def f(x: Int): Option[Int] = if (x == 0) None else Some(x)
  //     val a = Option(0)
  //     val b = Option(1)
  //     (a <+> b).flatMap(f) != (a.flatMap(f) <+> b.flatMap(f))
  //
  // def monadCombineLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => F[B]): IsEq[F[B]] =
  //   F.combineK(fa, fa2).flatMap(f) <-> F.combineK(fa flatMap f, fa2 flatMap f)
}

object MonadCombineLaws {
  def apply[F[_]](implicit ev: MonadCombine[F]): MonadCombineLaws[F] =
    new MonadCombineLaws[F] { def F: MonadCombine[F] = ev }
}
