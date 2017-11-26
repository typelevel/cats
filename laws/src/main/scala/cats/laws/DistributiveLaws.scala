package cats
package laws

import cats.Id
trait DistributiveLaws[F[_]] extends FunctorLaws[F] {
  implicit override def F: Distributive[F]

  def cosequenceIdentity[G[_], A](fa: F[A]): IsEq[F[A]] = {
    F.cosequence[Id, A](fa) <-> fa
  }

  //TODO need laws for 
  // cosequence andThen cosequence == id
  // composition 
}

object DistributiveLaws {
  def apply[F[_]](implicit ev: Distributive[F]): DistributiveLaws[F] =
    new DistributiveLaws[F] { def F: Distributive[F] = ev }
}
