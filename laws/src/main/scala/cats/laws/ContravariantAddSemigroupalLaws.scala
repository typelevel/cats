package cats
package laws

/**
 * Laws that are expected for any `cats.ContravariantAddSemigroupal`.
 */
trait ContravariantAddSemigroupalLaws[F[_]] extends ContravariantLaws[F] with InvariantAddSemigroupalLaws[F] {
  implicit override def I: ContravariantAddSemigroupal[F]
  implicit override def F: Contravariant[F] = I.contravariant

  def contramapSumRightDistributivity[A, B, C](fa: F[A], fb: F[B], f: C => A, g: C => B): IsEq[F[Either[C, C]]] =
    I.sum(F.contramap(fa)(f), F.contramap(fb)(g)) <->
      F.contramap(I.sum(fa, fb))(
        (e: Either[C, C]) =>
          e.fold(
            f.andThen(Left(_)),
            g.andThen(Right(_))
        )
      )

}
object ContravariantAddSemigroupalLaws {
  def apply[F[_]](implicit ev: ContravariantAddSemigroupal[F]): ContravariantAddSemigroupalLaws[F] =
    new ContravariantAddSemigroupalLaws[F] { def I: ContravariantAddSemigroupal[F] = ev }
}
