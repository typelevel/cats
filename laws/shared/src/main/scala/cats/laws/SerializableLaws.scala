package cats
package laws

import org.scalacheck.Prop

/**
 * Check for Java Serializability.
 */
object SerializableLaws {
  def serializable[A](a: A): Prop = Platform.serializable(a)
}
