package cats
package laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._
import Prop.{False, Proof, Result}

private[laws] object Platform {

  // Scala-js does not implement the Serializable interface, so we just return true.
  @inline
  def serializable[A](m: A): Prop = Prop { _ =>
    Result(status = Proof)
  }
}
