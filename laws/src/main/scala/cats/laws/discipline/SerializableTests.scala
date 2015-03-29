package cats.laws
package discipline

import org.typelevel.discipline.Laws

object SerializableTests extends Laws {
  def serializable[A](a: A): RuleSet =
    new RuleSet {
      def name = "serializable"
      def bases = Nil
      def parents = Nil
      def props = Seq(
        "can serialize and deserialize" -> SerializableLaws.serializable(a)
      )
    }
}
