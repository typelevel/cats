package cats.kernel
package laws

import org.typelevel.discipline.Laws
import org.scalacheck._
import org.scalacheck.Prop._

object HashLaws {
  def apply[A : Eq : Arbitrary : Cogen]: HashLaws[A] =
    new HashLaws[A] {
      def Equ = implicitly[Eq[A]]
      def Arb = implicitly[Arbitrary[A]]
      def Cog = implicitly[Cogen[A]]
    }
}

/**
 * @author Tongfei Chen
 */
trait HashLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]
  implicit def Cog: Cogen[A]

  def hash(implicit A: Hash[A]): HashProperties = new HashProperties(
    name = "hash",
    parent = None,
    Rules.serializable(Equ),
    "compatibility-hash" -> forAll { (x: A, y: A) =>
      !(A.eqv(x, y)) ?|| (A.hash(x) == A.hash(y))
    }
  )

  def sameAsUniversalHash(implicit A: Hash[A]): HashProperties = new HashProperties(
    name = "sameAsUniversalHash",
    parent = None,
    Rules.serializable(Equ),
    "same-as-universal-hash" -> forAll { (x: A) =>
      (A.hash(x) == x.##) && (Hash.fromUniversalHashCode[A].hash(x) == x.##)
    }
  )

  class HashProperties(name: String, parent: Option[RuleSet], props: (String, Prop)*)
  extends DefaultRuleSet(name, parent, props: _*)

}
