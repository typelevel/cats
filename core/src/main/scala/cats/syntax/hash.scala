package cats
package syntax

trait HashSyntax {

  implicit def catsSyntaxHash[A: Hash](a: A): HashOps[A] =
    new HashOps[A](a)

}

final class HashOps[A: Hash](a: A) {

  /**
   * Gets the hash code of this object given an implicit `Hash` instance.
   */
  def hash: Int = Hash[A].hash(a)
}
