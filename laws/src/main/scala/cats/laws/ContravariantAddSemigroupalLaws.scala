package cats
package laws

/**
 * Laws that are expected for any `cats.ContravariantChoice`.
 */
trait ContravariantChoiceLaws[F[_]] extends ContravariantLaws[F] with InvariantChoiceLaws[F] {
  implicit override def I: ContravariantChoice[F]
  implicit override def F: Contravariant[F] = I.contravariant

  def contramapChoiceRightDistributivity[A, B, C](fa: F[A], fb: F[B], f: C => A, g: C => B): IsEq[F[Either[C, C]]] =
    I.choice(F.contramap(fa)(f), F.contramap(fb)(g)) <->
      F.contramap(I.choice(fa, fb))((e: Either[C, C]) =>
        e.fold(
          f.andThen(Left(_)),
          g.andThen(Right(_))
        )
      )

}
object ContravariantChoiceLaws {
  def apply[F[_]](implicit ev: ContravariantChoice[F]): ContravariantChoiceLaws[F] =
    new ContravariantChoiceLaws[F] { def I: ContravariantChoice[F] = ev }
}
