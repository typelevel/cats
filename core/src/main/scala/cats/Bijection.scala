package cats

/**
 * Bijection is an invertible function.
 * 
 * It represents both (A => B) and its inverse, (B => A).
 * 
 * Laws:
 *  - inverse(apply(a)) = a
 *  - apply(inverse(b)) = b
 *  - 
 */
final case class Bijection[A, B](f: A => B, g: B => A) extends Function1[A, B] {
  def apply(a: A): B = f(a)
  def inverse(b: B): A = g(b)
  def invert: Bijection[B, A] = Bijection(g, f)

  def andThen[C](other: Bijection[B,C]): Bijection[A,C] =
    Bijection(f andThen other.f, g compose other.g)

  def compose[Z](other: Bijection[Z,A]): Bijection[Z,B] =
    Bijection(f compose other.f, g andThen other.g)
}

object Bijection {
  def identity[A] = Bijection[A, A](Predef.identity, Predef.identity)
}
