package cats.eq

import cats.Id

/**
 * A value of `A Is B` witnesses that the types `A` and `B` are the same.
 * You can use this value to coerce `A` into `B` and therefore `A Is B` is
 * similar in feel to `A =:= B`; however, `A Is B` has more computational
 * content and offers greater functionality.
 *
 * `A Is B` is also known as Leibniz equality and represents computationally
 * the idea that two things are equal if they take equal values on all
 * predicates. In other words, `A` equals `B` when for all predicates `P[_]`
 * we have that `P[A]` implies `P[B]` and visa versa. `A Is B` represents
 * this idea computationally providing exploitable structure to our equalities.
 *
 * The heart of `A Is B` is the `subst` method which converts values `F[A]`
 * into values `F[B]` for any `F[_]`. Creative choices of `F` enable type
 * equalities to be substituted into many more complex type expressions.
 */
abstract class Is[A, B] {
  def subst[F[_]](fa: F[A]): F[B]

  final def andThen[C](next: B Is C): A Is C =
    next.subst[A Is ?](this)

  final def compose[C](prev: C Is A): C Is B =
    prev andThen this

  final def from: B Is A =
    this.subst[? Is A](Is.refl)

  final def lift[F[_]]: F[A] Is F[B] =
    subst[λ[α => F[A] Is F[α]]](Is.refl)

  /**
   * Substitution on identity brings about a direct coercion function of the
   * same form that `=:=` provides.
   */
  def coerce: A => B =
    subst[Id]
}

object Is {

  /**
   * In normal circumstances the only `Is` value which is available is the
   * computational identity `A Is A` at all types `A`. These "self loops"
   * generate all of the behavior of equality and also ensure that at its
   * heart `A Is B` is always just an identity relation.
   */
  implicit def refl[A]: A Is A = new Is[A, A] {
    def subst[F[_]](fa: F[A]): F[A] = fa
  }

  /**
   * A value `A Is B` is always sufficient to produce a similar `Predef.=:=`
   * value.
   */
  implicit def scalaEq[A, B](t: A Is B): A =:= B =
    t.subst[A =:= ?](implicitly[A =:= A])

}