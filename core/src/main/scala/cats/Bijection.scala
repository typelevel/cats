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

  def compose[C](other: Bijection[B,C]): Bijection[A,C] =
    Bijection(other.f compose f, g compose other.g)
}
