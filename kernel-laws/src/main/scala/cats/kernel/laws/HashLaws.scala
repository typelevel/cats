package cats.kernel
package laws

import org.typelevel.discipline._
import org.scalacheck._
import org.scalacheck.Prop._

import scala.util.hashing._

object HashLaws {
  def apply[A : Eq : Arbitrary]: HashLaws[A] =
    new HashLaws[A] {
      def Equ = implicitly[Eq[A]]
      def Arb = implicitly[Arbitrary[A]]
    }
}

/**
 * @author Tongfei Chen
 */
trait HashLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def hash(implicit A: Hash[A]): HashProperties = new HashProperties(
    name = "hash",
    parent = None,
    Rules.serializable(Equ),
    "compatibility-hash" -> forAll { (x: A, y: A) =>
      !(A.eqv(x, y)) ?|| (Hash.hash(x) == Hash.hash(y))
    }
  )

  def sameAsUniversalHash(implicit A: Hash[A]): HashProperties = new HashProperties(
    name = "sameAsUniversalHash",
    parent = None,
    Rules.serializable(Equ),
    "same-as-universal-hash" -> forAll { (x: A, y: A) =>
      (A.hash(x) == x.##) && (Hash.fromUniversalHashCode[A].hash(x) == x.##) &&
        (!(A.eqv(x, y)) || (Hash.fromUniversalHashCode[A].eqv(x, y)))
    }
  )

  def sameAsScalaHashing(implicit catsHash: Hash[A], scalaHashing: Hashing[A]): HashProperties = new HashProperties(
    name = "sameAsScalaHashing",
    parent = None,
    Rules.serializable(Equ),
    "same-as-scala-hashing" -> forAll { (x: A) =>
      catsHash.hash(x) == Hash.fromHashing(scalaHashing).hash(x)
    }
  )

  class HashProperties(name: String, parent: Option[RuleSet], props: (String, Prop)*)
  extends DefaultRuleSet(name, parent, props: _*)

}
