package cats
package laws
package discipline

import org.typelevel.discipline.Laws
import org.scalacheck.Prop
import Prop._

object SerializableTests extends Laws {
  def serializable[A](a: A): RuleSet =
    new DefaultRuleSet(
      name = "serializable",
      parent = None,
      "can serialize and deserialize" -> SerializableLaws.serializable(a))
}
