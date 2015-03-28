package cats.laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import org.typelevel.discipline.Laws

trait SerializableTests extends Laws {
  def serializable[A : Arbitrary]: RuleSet =
    new RuleSet {
      def name = "serializable"
      def bases = Nil
      def parents = Nil
      def props = Seq(
        "can serialize and deserialize" -> forAll(SerializableLaws.serializable[A] _)
      )
    }
}
