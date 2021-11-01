package alleycats.laws.discipline

import cats.kernel.{Eq, Hash}
import cats.kernel.laws.HashLaws
import cats.kernel.laws.discipline._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import scala.util.hashing.Hashing

trait SystemIdentityHashTests[A] extends ReferentialEqTests[A] {
  def laws: HashLaws[A]

  def hash(implicit arbA: Arbitrary[A], eqA: Eq[A], hashA: Hashing[A]): RuleSet =
    new DefaultRuleSet(
      "systemIdentityHash",
      Some(eqv),
      "hash compatibility" -> forAll(laws.hashCompatibility _),
      "same as universal hash" -> forAll(laws.sameAsUniversalHash _),
      "same as scala hashing" -> forAll((x: A, y: A) => laws.sameAsScalaHashing(x, y, hashA))
    )
}

object SystemIdentityHashTests {
  def apply[A: Hash]: SystemIdentityHashTests[A] = new SystemIdentityHashTests[A] {
    override def laws: HashLaws[A] = HashLaws[A]
  }
}
