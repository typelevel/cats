package alleycats

import cats.Hash

/**
 * A `Hash[A]` that delegates to identity hashcode (`System.identityHashCode`).
 * It implements `Eq[A]` via referential equality (`eq`) that it is not referentially transparent!
 */
trait SystemIdentityHash[A <: AnyRef] extends ReferentialEq[A] with Hash[A] {
  override def hash(a: A): Int = java.lang.System.identityHashCode(a)
}

object SystemIdentityHash {
  private[this] val identityHash: Hash[AnyRef] = new SystemIdentityHash[AnyRef] {}

  def apply[A <: AnyRef]: Hash[A] = identityHash.asInstanceOf[Hash[A]]
}
