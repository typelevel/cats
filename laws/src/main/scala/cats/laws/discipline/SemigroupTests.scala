package cats
package laws
package discipline

import algebra.laws._
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

object SemigroupTests {
  def apply[A: Arbitrary](implicit eqa: Eq[A]): SemigroupTests[A] =
    new SemigroupTests[A] {
      def EqA = eqa
      def ArbA = implicitly[Arbitrary[A]]
    }
}

trait SemigroupTests[A] extends Laws {
  implicit def EqA: Eq[A]
  implicit def ArbA: Arbitrary[A]

  def associative(implicit A: Semigroup[A]) = {
    val laws = SemigroupLaws[A]
    new SemigroupProperties(
      name = "semigroup",
      parents = Nil,
      "associative" -> forAll { (a: A, b: A, c: A) =>
        val (lhs, rhs) = laws.associative(a, b, c)
        lhs ?== rhs
      })
  }

  class SemigroupProperties(
    val name: String,
    val parents: Seq[SemigroupProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Nil
  }
}
