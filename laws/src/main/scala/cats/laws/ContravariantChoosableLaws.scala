package cats
package laws

/**
 * Laws that are expected for any `cats.ContravariantChoosable`.
 */
trait ContravariantChoosableLaws[F[_]] extends ContravariantChoiceLaws[F] with InvariantChoosableLaws[F] {
  implicit override def I: ContravariantChoosable[F]
}
object ContravariantChoosableLaws {
  def apply[F[_]](implicit ev: ContravariantChoosable[F]): ContravariantChoosableLaws[F] =
    new ContravariantChoosableLaws[F] { def I: ContravariantChoosable[F] = ev }
}
