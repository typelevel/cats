package cats
package laws
package discipline

import algebra.laws._
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

object MonoidTests {
  def apply[A: Arbitrary](implicit eqa: Eq[A]): MonoidTests[A] =
    new MonoidTests[A] {
      def EqA = eqa
      def ArbA = implicitly[Arbitrary[A]]
    }
}

trait MonoidTests[A] extends SemigroupTests[A] {
  def identity(implicit A: Monoid[A]) = {
    val laws = MonoidLaws[A]
    new SemigroupProperties(
      name = "monoid",
      parents = Seq(associative),
      "left identity" -> forAll { (a: A) =>
        val (lhs, rhs) = laws.leftIdentity(a)
        lhs ?== rhs
      },
      "right identity" -> forAll { (a: A) =>
        val (lhs, rhs) = laws.rightIdentity(a)
        lhs ?== rhs
      })
  }
}
