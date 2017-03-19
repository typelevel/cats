package cats
package evidence

import arrow.Category

/**
 * As substitutability: A better `<:<`
 *
 * This class exists to aid in the capture proof of subtyping
 * relationships, which can be applied in other context to widen other
 * type
 *
 * `A As B` holds whenever `A` could be used in any negative context
 * that expects a `B`.  (e.g. if you could pass an `A` into any
 * function that expects a `B` as input.)
 *
 * This code was ported directly from scalaz to cats using this version from scalaz:
 * https://github.com/scalaz/scalaz/blob/a89b6d63/core/src/main/scala/scalaz/As.scala
 *
 *  The original contribution to scalaz came from Jason Zaugg
 */
sealed abstract class As[-A, +B] extends Serializable {
  /**
   * Use this subtyping relationship to replace B with a value of type
   * A in a contravariant context.  This would commonly be the input
   * to a function, such as F in: `type F[-B] = B => String`. In this
   * case, we could use A As B to turn an F[B] Into F[A].
   */
  def substitute[F[-_]](p: F[B]): F[A]

  @inline final def andThen[C](that: (B As C)): (A As C) = As.compose(that, this)

  @inline final def compose[C](that: (C As A)): (C As B) = As.compose(this, that)

  @inline final def coerce(a: A): B = As.witness(this)(a)
}

sealed abstract class AsInstances {
  import As._

  /*
   * Subtyping forms a category
   */
  implicit val liskov: Category[As] = new Category[As] {
    def id[A]: (A As A) = refl[A]

    def compose[A, B, C](bc: B As C, ab: A As B): (A As C) = bc compose ab
  }
}

object As extends AsInstances {
  /**
   * In truth, "all values of `A Is B` are `refl`". `reflAny` is that
   * single value.
   */
  private[this] val reflAny = new (Any As Any) {
    def substitute[F[-_]](fa: F[Any]) = fa
  }
  /**
   * Subtyping is reflexive
   */
  implicit def refl[A]: (A As A) =
    reflAny.asInstanceOf[A As A]

  /**
   * We can witness the relationship by using it to make a substitution *
   */
  implicit def witness[A, B](lt: A <~< B): A => B =
    lt.substitute[-? => B](identity)

  /**
   * Subtyping is transitive
   */
  def compose[A, B, C](f: B <~< C, g: A <~< B): A <~< C =
    g.substitute[λ[`-α` => α <~< C]](f)

  /**
   * Lift Scala's subtyping relationship
   */
  @inline def unsafeFromPredef[A, B >: A]: (A As B) =
    reflAny.asInstanceOf[A As B]

  /**
   * We can lift subtyping into any covariant type constructor
   */
  def co[T[+_], A, A2] (a: A As A2): (T[A] As T[A2]) =
    a.substitute[λ[`-α` => T[α] As T[A2]]](refl)

  // Similarly, we can do this any time we find a covariant type
  // parameter. Here we provide the proof for what we expect to be the
  // most common shapes.

  def co2[T[+_, _], Z, A, B](a: A As Z): T[A, B] As T[Z, B] =
    a.substitute[λ[`-α` => T[α, B] As T[Z, B]]](refl)

  /**
   * Widen a F[X,+A] to a F[X,B] if (A As B). This can be used to widen
   * the output of a Function1, for example.
   */
  def co2_2[T[_, +_], Z, A, B](a: B As Z): T[A, B] As T[A, Z] =
    a.substitute[λ[`-α` => T[A, α] As T[A, Z]]](refl)

  def co3[T[+_, _, _], Z, A, B, C](a: A As Z): T[A, B, C] As T[Z, B, C] =
    a.substitute[λ[`-α` => T[α, B, C] As T[Z, B, C]]](refl)

  /**
   * Use this relationship to widen the output type of a Function1
   */
  def onF[X, A, B](ev: A As B)(fa: X => A): X => B = co2_2[Function1, B, X, A](ev).coerce(fa)

  /**
   * widen two types for binary type constructors covariant in both
   * parameters
   *
   * lift2(a,b) = co1_2(a) compose co2_2(b)
   */
  def lift2[T[+_, +_], A, A2, B, B2](
    a: A As A2,
    b: B As B2
  ): (T[A, B] As T[A2, B2]) = {
    type a[-X] = T[X, B2] As T[A2, B2]
    type b[-X] = T[A, X] As T[A2, B2]
    b.substitute[b](a.substitute[a](refl))
  }

  /**
   *  We can lift a subtyping relationship into a contravariant type
   *  constructor.
   *
   *  Given that F has the shape: F[-_], we show that:
   *     (A As B) implies (F[B] As F[A])
   */
  def contra[T[-_], A, B](a: A As B): (T[B] As T[A]) =
    a.substitute[λ[`-α` => T[B] As T[α]]](refl)

  // Similarly, we can do this any time we find a contravariant type
  // parameter. Here we provide the proof for what we expect to be the
  // most common shapes.

  // binary
  def contra1_2[T[-_, _], Z, A, B](a: A As Z): (T[Z, B] As T[A, B]) =
    a.substitute[λ[`-α` => T[Z, B] As T[α, B]]](refl)

  def contra2_2[T[_, -_], Z, A, B](a: B As Z): (T[A, Z] As T[A, B]) =
    a.substitute[λ[`-α` => T[A, Z] As T[A, α]]](refl)

  // ternary
  def contra1_3[T[-_, _, _], Z, A, B, C](a: A As Z): (T[Z, B, C] As T[A, B, C]) =
    a.substitute[λ[`-α` => T[Z, B, C] As T[α, B, C]]](refl)

  def contra2_3[T[_, -_, _], Z, A, B, C](a: B As Z): (T[A, Z, C] As T[A, B, C]) =
    a.substitute[λ[`-α` => T[A, Z, C] As T[A, α, C]]](refl)

  def contra3_3[T[_, _, -_], Z, A, B, C](a: C As Z): (T[A, B, Z] As T[A, B, C]) =
    a.substitute[λ[`-α` => T[A, B, Z] As T[A, B, α]]](refl)

  /**
   *  Lift subtyping into a Function1-like type
   *  liftF1(a,r) = contra1_2(a) compose co2_2(b)
   */
  def liftF1[F[-_, +_], A, A2, R, R2](
    a: A As A2,
    r: R As R2
  ): (F[A2, R] As F[A, R2]) = {
    type a[-X] = F[A2, R2] As F[X, R2]
    type r[-X] = F[A2, X] As F[A, R2]
    r.substitute[r](a.substitute[a](refl))
  }

  /**
   * Lift subtyping into a function
   */
  def liftF2[F[-_, -_, +_], A, A2, B, B2, R, R2](
    a: A As A2,
    b: B As B2,
    r: R As R2
  ): (F[A2, B2, R] As F[A, B, R2]) = {
    type a[-X] = F[A2, B2, R2] As F[X, B2, R2]
    type b[-X] = F[A2, B2, R2] As F[A, X, R2]
    type r[-X] = F[A2, B2, X] As F[A, B, R2]
    r.substitute[r](b.substitute[b](a.substitute[a](refl)))
  }

  /**
   * Lift subtyping into a function
   * liftF3(a,b,c,r) = contra1_4(a) compose contra2_4(b) compose contra3_4(c) compose co3_4(d)
   */
  def liftF3[F[-_, -_, -_, +_], A, A2, B, B2, C, C2, R, R2](
     a: A As A2,
     b: B As B2,
     c: C As C2,
     r: R As R2
  ): (F[A2, B2, C2, R] As F[A, B, C, R2]) = {
    type a[-X] = F[A2, B2, C2, R2] As F[X, B2, C2, R2]
    type b[-X] = F[A2, B2, C2, R2] As F[A, X, C2, R2]
    type c[-X] = F[A2, B2, C2, R2] As F[A, B, X, R2]
    type r[-X] = F[A2, B2, C2, X] As F[A, B, C, R2]
    r.substitute[r](c.substitute[c](b.substitute[b](a.substitute[a](refl))))
  }

}
