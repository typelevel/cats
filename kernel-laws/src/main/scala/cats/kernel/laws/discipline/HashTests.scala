package cats
package kernel
package laws
package discipline

import cats.kernel.instances.boolean._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import scala.util.hashing.Hashing

trait HashTests[A] extends EqTests[A] {

  def laws: HashLaws[A]

  def hash(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqA: Eq[A], hashA: Hashing[A]): RuleSet =
    new DefaultRuleSet(
      "hash",
      Some(eqv),
      "hash compatibility" -> forAll(laws.hashCompatibility _),
      "same as universal hash" -> forAll(laws.sameAsUniversalHash _),
      "same as scala hashing" -> forAll((x: A, y: A) => laws.sameAsScalaHashing(x, y, hashA))
    )

}

object HashTests {
  def apply[A: Hash]: HashTests[A] =
    new HashTests[A] { def laws: HashLaws[A] = HashLaws[A] }
}
