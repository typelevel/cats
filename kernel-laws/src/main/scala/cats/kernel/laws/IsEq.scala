package cats.kernel
package laws

/** Represents two values of the same type that are expected to be equal. */
final case class IsEq[A](lhs: A, rhs: A)
