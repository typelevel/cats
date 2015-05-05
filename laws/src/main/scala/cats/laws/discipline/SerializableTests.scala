package cats
package laws
package discipline

import org.typelevel.discipline.Laws
import org.scalacheck.Prop
import Prop._

object SerializableTests extends Laws {
  def serializable[A](a: A): RuleSet =
    new RuleSet {
      def name: String = "serializable"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Nil
      def props: Seq[(String, Prop)] = Seq(
        "can serialize and deserialize" -> SerializableLaws.serializable(a)
      )
    }
}
