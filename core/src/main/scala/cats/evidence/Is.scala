package cats.evidence

import cats.Id

/**
 * A value of `A Is B` is proof that the types `A` and `B` are the same. More
 * powerfully, it asserts that they have the same meaning in all type
 * contexts. This can be a more powerful assertion than `A =:= B` and is more
 * easily used in manipulation of types while avoiding (potentially
 * erroneous) coercions.
 *
 * `A Is B` is also known as Leibniz equality.
 */
abstract class Is[A, B] {
  def subst[F[_]](fa: F[A]): F[B]

  @inline final def andThen[C](next: B Is C): A Is C =
    next.subst[A Is ?](this)

  @inline final def compose[C](prev: C Is A): C Is B =
    prev andThen this

  @inline final def from: B Is A =
    this.subst[? Is A](Is.refl)

  @inline final def lift[F[_]]: F[A] Is F[B] =
    subst[λ[α => F[A] Is F[α]]](Is.refl)

  /**
   * Substitution on identity brings about a direct coercion function of the
   * same form that `=:=` provides.
   */
  @inline final def coerce(a: A): B =
    subst[Id](a)
}

object Is {

  /**
   * In truth, "all values of `A Is B` are `refl`". `reflAny` is that
   * single value.
   */
  private[this] val reflAny = new Is[Any, Any] {
    def subst[F[_]](fa: F[Any]) = fa
  }

  /**
   * In normal circumstances the only `Is` value which is available is the
   * computational identity `A Is A` at all types `A`. These "self loops"
   * generate all of the behavior of equality and also ensure that at its
   * heart `A Is B` is always just an identity relation.
   *
   * Implementation note: all values of `refl` return the same (private)
   * instance at whatever type is appropriate to save on allocations.
   */
  implicit def refl[A]: A Is A =
    reflAny.asInstanceOf[A Is A]

  /**
   * A value `A Is B` is always sufficient to produce a similar `Predef.=:=`
   * value.
   */
  implicit def predefEq[A, B](t: A Is B): A =:= B =
    t.subst[A =:= ?](implicitly[A =:= A])

}

