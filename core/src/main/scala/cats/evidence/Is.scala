/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.evidence

import cats.Id
import cats.arrow.*

/**
 * A value of `A Is B` is proof that the types `A` and `B` are the same. More
 * powerfully, it asserts that they have the same meaning in all type
 * contexts. This can be a more powerful assertion than `A =:= B` and is more
 * easily used in manipulation of types while avoiding (potentially
 * erroneous) coercions.
 *
 * `A Is B` is also known as Leibniz equality.
 */
abstract class Is[A, B] extends Serializable {

  /**
   * To create an instance of `A Is B` you must show that for every choice
   * of `F[_]` you can convert `F[A]` to `F[B]`. Loosely, this reads as
   * saying that `B` must have the same effect as `A` in all contexts
   * therefore allowing type substitution.
   */
  def substitute[F[_]](fa: F[A]): F[B]

  /**
   * `Is` is transitive and therefore values of `Is` can be composed in a
   * chain much like functions. See also `compose`.
   */
  @inline final def andThen[C](next: B Is C): A Is C =
    next.substitute[Is[A, *]](this)

  /**
   * `Is` is transitive and therefore values of `Is` can be composed in a
   * chain much like functions. See also `andThen`.
   */
  @inline final def compose[C](prev: C Is A): C Is B =
    prev.andThen(this)

  /**
   * `Is` is symmetric and therefore can be flipped around. Flipping is its
   * own inverse, so `x.flip.flip == x`.
   */
  @inline final def flip: B Is A =
    this.substitute[Is[*, A]](Is.refl)

  /**
   * Sometimes for more complex substitutions it helps the typechecker to
   * wrap one layer of `F[_]` context around the types you're equating
   * before substitution.
   */
  @inline final def lift[F[_]]: F[A] Is F[B] =
    substitute[λ[α => F[A] Is F[α]]](Is.refl)

  /**
   * Substitution on identity brings about a direct coercion function of the
   * same form that `=:=` provides.
   */
  @inline final def coerce(a: A): B =
    substitute[Id](a)

  /**
   * A value `A Is B` is always sufficient to produce a similar `Predef.=:=`
   * value.
   */
  @deprecated("Use toPredef for consistency with As", "2.2.0")
  @inline final def predefEq: A =:= B =
    substitute[=:=[A, *]](implicitly[A =:= A])

  /**
   * A value `A Is B` is always sufficient to produce a similar `Predef.=:=`
   * value.
   */
  @inline final def toPredef: A =:= B =
    substitute[=:=[A, *]](implicitly[A =:= A])
}

sealed abstract class IsInstances {
  import Is.*

  /**
   * The category instance on Leibniz categories.
   */
  implicit val leibniz: Category[Is] = new Category[Is] {
    def id[A]: A Is A = refl[A]
    def compose[A, B, C](bc: B Is C, ab: A Is B): A Is C = bc.compose(ab)
  }
}

object Is extends IsInstances with IsSupport {

  /**
   * In truth, "all values of `A Is B` are `refl`". `reflAny` is that
   * single value.
   */
  private[this] val reflAny = new Is[Any, Any] {
    def substitute[F[_]](fa: F[Any]) = fa
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
  @inline implicit def refl[A]: A Is A =
    reflAny.asInstanceOf[A Is A]

  /**
   * It can be convenient to convert a `Predef.=:=` value into an `Is` value.
   * This is not actually unsafe, but was previously labeled as such out
   * of an abundance of caution
   */
  @deprecated("use Is.isFromPredef", "2.2.0")
  @inline def unsafeFromPredef[A, B](eq: A =:= B): A Is B =
    Is.isFromPredef(eq)
}
