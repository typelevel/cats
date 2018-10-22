package cats.kernel
package laws

import scala.util.hashing._

trait HashLaws[A] extends EqLaws[A] {
  implicit override def E: Hash[A]

  def hashCompatibility(x: A, y: A): IsEq[Boolean] =
    (!E.eqv(x, y) || (Hash.hash(x) == Hash.hash(y))) <-> true

  def sameAsUniversalHash(x: A, y: A): IsEq[Boolean] =
    ((E.hash(x) == x.hashCode) && (Hash.fromUniversalHashCode[A].hash(x) == x.hashCode()) &&
      (E.eqv(x, y) == Hash.fromUniversalHashCode[A].eqv(x, y))) <-> true

  def sameAsScalaHashing(x: A, y: A, scalaHashing: Hashing[A]): IsEq[Boolean] =
    ((E.hash(x) == Hash.fromHashing(scalaHashing).hash(x)) &&
      (E.eqv(x, y) == Hash.fromHashing(scalaHashing).eqv(x, y))) <-> true

}

object HashLaws {
  def apply[A](implicit ev: Hash[A]): HashLaws[A] =
    new HashLaws[A] { def E: Hash[A] = ev }
}
