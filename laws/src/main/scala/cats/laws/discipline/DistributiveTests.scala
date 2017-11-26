package cats
package laws
package discipline

trait DistributiveTests[F[_]] extends FunctorTests[F] {
  def laws: DistributiveLaws[F]
}

object DistributiveTests {
  def apply[F[_]: Distributive]: DistributiveTests[F] =
    new DistributiveTests[F] { def laws: DistributiveLaws[F] = DistributiveLaws[F] }
}
