package cats
package laws


/**
  * Laws that are expected for any `cats.InvariantSemigroupal`.
  */
trait InvariantSemigroupalLaws[F[_]] extends InvariantLaws[F] with SemigroupalLaws[F] {
  implicit override def F: InvariantSemigroupal[F]

}
object InvariantSemigroupalLaws {
  def apply[F[_]](implicit ev: InvariantSemigroupal[F]): InvariantSemigroupalLaws[F] =
    new InvariantSemigroupalLaws[F] { def F: InvariantSemigroupal[F] = ev }
}
