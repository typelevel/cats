package alleycats

import cats.Hash

class SystemIdentityHash[A <: AnyRef] extends ReferentialEq[A] with Hash[A] {
  override def hash(a: A): Int = java.lang.System.identityHashCode(a)
}

object SystemIdentityHash {
  private[this] val identityHash: Hash[AnyRef] = new SystemIdentityHash[AnyRef]

  def apply[A <: AnyRef]: Hash[A] = identityHash.asInstanceOf[Hash[A]]
}