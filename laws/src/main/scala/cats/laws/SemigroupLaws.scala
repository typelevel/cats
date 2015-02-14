package cats
package laws

/**
 * Laws that must be obeyed by any [[cats.Semigroup]].
 */
trait SemigroupLaws[A] {
  implicit def A: Semigroup[A]

  def associative(a: A, b: A, c: A): (A, A) =
   A.combine(A.combine(a, b), c) -> A.combine(a, A.combine(b, c))
}

object SemigroupLaws {
  def apply[A](implicit ev: Semigroup[A]): SemigroupLaws[A] =
    new SemigroupLaws[A] { def A = ev }
}
