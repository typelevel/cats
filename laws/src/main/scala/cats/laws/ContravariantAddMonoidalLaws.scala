package cats
package laws

/**
 * Laws that are expected for any `cats.ContravariantAddSemigroupal`.
 */
trait ContravariantAddMonoidalLaws[F[_]] extends ContravariantAddSemigroupalLaws[F] with InvariantAddMonoidalLaws[F] {
  implicit override def I: ContravariantAddMonoidal[F]
}
object ContravariantAddMonoidalLaws {
  def apply[F[_]](implicit ev: ContravariantAddMonoidal[F]): ContravariantAddMonoidalLaws[F] =
    new ContravariantAddMonoidalLaws[F] { def I: ContravariantAddMonoidal[F] = ev }
}
