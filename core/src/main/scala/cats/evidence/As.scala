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
sealed abstract class As[-A, +B] {
  def apply(a: A): B = As.witness(this)(a)

  /**
   * Use this subtyping relationship to replace B with a value of type
   * A in a contravariant context.  This would commonly be the input
   * to a function, such as F in: `type F[-B] = B => String`. In this
   * case, we could use A As B to turn an F[B] Into F[A].
   */
  def subst[F[-_]](p: F[B]): F[A]

  final def andThen[C](that: As[B, C]): As[A, C] = As.trans(that, this)

  final def compose[C](that: As[C, A]): As[C, B] = As.trans(this, that)

  /**
   * Use this relationship to widen the output type of a Function1
   */
  def onF[X](fa: X => A): X => B = As.co2_2[Function1, B, X, A](this)(fa)
}

sealed abstract class AsInstances {
  import As._

  /*
   * Subtyping forms a category 
   */
  implicit val liskov: Category[<~<] = new Category[<~<] {
    def id[A]: (A <~< A) = refl[A]

    def compose[A, B, C](bc: B <~< C, ab: A <~< B): (A <~< C) = trans(bc, ab)
  }
}

object As extends AsInstances {
  /**
   * Lift Scala's subtyping relationship 
   */
  implicit def isa[A, B >: A]: A <~< B =
    new (A <~< B) {
      def subst[F[-_]](p: F[B]): F[A] = p
    }

  /**
   * We can witness the relationship by using it to make a substitution *
   */
  implicit def witness[A, B](lt: A <~< B): A => B = 
    lt.subst[-? => B](identity)

  /**
   * Subtyping is reflexive 
   */
  implicit def refl[A]: (A <~< A) =
    new (A <~< A) {
      def subst[F[-_]](p: F[A]): F[A] = p
    }

  /**
   * Subtyping is transitive 
   */
  def trans[A, B, C](f: B <~< C, g: A <~< B): A <~< C =
    g.subst[λ[`-α` => α <~< C]](f)

  /**
   * We can lift subtyping into any covariant type constructor 
   */
  def co[T[+_], A, A2](a: A <~< A2): (T[A] <~< T[A2]) =
    a.subst[λ[`-α` => T[α] <~< T[A2]]](refl)

  // Similarly, we can do this any time we find a covariant type
  // parameter. Here we provide the proof for what we expect to be the
  // most common shapes.

  def co2[T[+_, _], Z, A, B](a: A <~< Z): T[A, B] <~< T[Z, B] =
    a.subst[λ[`-α` => T[α, B] <~< T[Z, B]]](refl)

  /**
   * Widen a F[X,+A] to as F[X,B] if A <~< B, this can be used to widen
   * the output of a Function1, for example
   */
  def co2_2[T[_, +_], Z, A, B](a: B <~< Z): T[A, B] <~< T[A, Z] =
    a.subst[λ[`-α` => T[A, α] <~< T[A, Z]]](refl)

  def co3[T[+_, _, _], Z, A, B, C](a: A <~< Z): T[A, B, C] <~< T[Z, B, C] =
    a.subst[λ[`-α` => T[α, B, C] <~< T[Z, B, C]]](refl)

  def co4[T[+_, _, _, _], Z, A, B, C, D](a: A <~< Z): T[A, B, C, D] <~< T[Z, B, C, D] =
    a.subst[λ[`-α` => T[α, B, C, D] <~< T[Z, B, C, D]]](refl)

  /**
   * widen two types for binary type constructors covariant in both
   * parameters 
   *  
   * lift2(a,b) = co1_2(a) compose co2_2(b)
   */
  def lift2[T[+_, +_], A, A2, B, B2](
    a: A <~< A2,
    b: B <~< B2
  ): (T[A, B] <~< T[A2, B2]) = {
    type a[-X] = T[X, B2] <~< T[A2, B2]
    type b[-X] = T[A, X] <~< T[A2, B2]
    b.subst[b](a.subst[a](refl))
  }

  /**
   * lift3(a,b,c) = co1_3(a) compose co2_3(b) compose co3_3(c) 
   */
  def lift3[T[+_, +_, +_], A, A2, B, B2, C, C2](
    a: A <~< A2,
    b: B <~< B2,
    c: C <~< C2
  ): (T[A, B, C] <~< T[A2, B2, C2]) = {
    type a[-X] = T[X, B2, C2] <~< T[A2, B2, C2]
    type b[-X] = T[A, X, C2] <~< T[A2, B2, C2]
    type c[-X] = T[A, B, X] <~< T[A2, B2, C2]
    c.subst[c](b.subst[b](a.subst[a](refl)))
  }

  /**
   * lift4(a,b,c,d) = co1_3(a) compose co2_3(b) compose co3_3(c) compose co4_4(d) 
   */
  def lift4[T[+_, +_, +_, +_], A, A2, B, B2, C, C2, D, D2](
    a: A <~< A2,
    b: B <~< B2,
    c: C <~< C2,
    d: D <~< D2
  ): (T[A, B, C, D] <~< T[A2, B2, C2, D2]) = {
    type a[-X] = T[X, B2, C2, D2] <~< T[A2, B2, C2, D2]
    type b[-X] = T[A, X, C2, D2] <~< T[A2, B2, C2, D2]
    type c[-X] = T[A, B, X, D2] <~< T[A2, B2, C2, D2]
    type d[-X] = T[A, B, C, X] <~< T[A2, B2, C2, D2]
    d.subst[d](c.subst[c](b.subst[b](a.subst[a](refl))))
  }

  /**
   *  We can lift a subtyping relationship into a contravariant type
   *  constructor.
   *  
   *  Given that F has the shape: F[-_], we show that:
   *     (A <~< B) implies (F[B] <~< F[A])
   */
  def contra[T[-_], A, B](a: A <~< B): (T[B] <~< T[A]) =
    a.subst[λ[`-α` => T[B] <~< T[α]]](refl)

  // Similarly, we can do this any time we find a contravariant type
  // parameter. Here we provide the proof for what we expect to be the
  // most common shapes.

  // binary
  def contra1_2[T[-_, _], Z, A, B](a: A <~< Z): (T[Z, B] <~< T[A, B]) =
    a.subst[λ[`-α` => T[Z, B] <~< T[α, B]]](refl)

  def contra2_2[T[_, -_], Z, A, B](a: B <~< Z): (T[A, Z] <~< T[A, B]) =
    a.subst[λ[`-α` => T[A, Z] <~< T[A, α]]](refl)

  // ternary
  def contra1_3[T[-_, _, _], Z, A, B, C](a: A <~< Z): (T[Z, B, C] <~< T[A, B, C]) =
    a.subst[λ[`-α` => T[Z, B, C] <~< T[α, B, C]]](refl)

  def contra2_3[T[_, -_, _], Z, A, B, C](a: B <~< Z): (T[A, Z, C] <~< T[A, B, C]) =
    a.subst[λ[`-α` => T[A, Z, C] <~< T[A, α, C]]](refl)

  def contra3_3[T[_, _, -_], Z, A, B, C](a: C <~< Z): (T[A, B, Z] <~< T[A, B, C]) =
    a.subst[λ[`-α` => T[A, B, Z] <~< T[A, B, α]]](refl)

  def contra1_4[T[-_, _, _, _], Z, A, B, C, D](a: A <~< Z): (T[Z, B, C, D] <~< T[A, B, C, D]) =
    a.subst[λ[`-α` => T[Z, B, C, D] <~< T[α, B, C, D]]](refl)

  def contra2_4[T[_, -_, _, _], Z, A, B, C, D](a: B <~< Z): (T[A, Z, C, D] <~< T[A, B, C, D]) =
    a.subst[λ[`-α` => T[A, Z, C, D] <~< T[A, α, C, D]]](refl)

  def contra3_4[T[_, _, -_, _], Z, A, B, C, D](a: C <~< Z): (T[A, B, Z, D] <~< T[A, B, C, D]) =
    a.subst[λ[`-α` => T[A, B, Z, D] <~< T[A, B, α, D]]](refl)

  def contra4_4[T[_, _, _, -_], Z, A, B, C, D](a: D <~< Z): (T[A, B, C, Z] <~< T[A, B, C, D]) =
    a.subst[λ[`-α` => T[A, B, C, Z] <~< T[A, B, C, α]]](refl)

  /**
   *  Lift subtyping into a Function1-like type
   *  liftF1(a,r) = contra1_2(a) compose co2_2(b)
   */
  def liftF1[F[-_, +_], A, A2, R, R2](
    a: A <~< A2,
    r: R <~< R2
  ): (F[A2, R] <~< F[A, R2]) = {
    type a[-X] = F[A2, R2] <~< F[X, R2]
    type r[-X] = F[A2, X] <~< F[A, R2]
    r.subst[r](a.subst[a](refl))
  }

  /**
   * Lift subtyping into a function
   */
  def liftF2[F[-_, -_, +_], A, A2, B, B2, R, R2](
    a: A <~< A2,
    b: B <~< B2,
    r: R <~< R2
  ): (F[A2, B2, R] <~< F[A, B, R2]) = {
    type a[-X] = F[A2, B2, R2] <~< F[X, B2, R2]
    type b[-X] = F[A2, B2, R2] <~< F[A, X, R2]
    type r[-X] = F[A2, B2, X] <~< F[A, B, R2]
    r.subst[r](b.subst[b](a.subst[a](refl)))
  }

  /**
   * Lift subtyping into a function
   * liftF3(a,b,c,r) = contra1_4(a) compose contra2_4(b) compose contra3_4(c) compose co3_4(d)
   */
  def liftF3[F[-_, -_, -_, +_], A, A2, B, B2, C, C2, R, R2](
     a: A <~< A2,
     b: B <~< B2,
     c: C <~< C2,
     r: R <~< R2
  ): (F[A2, B2, C2, R] <~< F[A, B, C, R2]) = {
    type a[-X] = F[A2, B2, C2, R2] <~< F[X, B2, C2, R2]
    type b[-X] = F[A2, B2, C2, R2] <~< F[A, X, C2, R2]
    type c[-X] = F[A2, B2, C2, R2] <~< F[A, B, X, R2]
    type r[-X] = F[A2, B2, C2, X] <~< F[A, B, C, R2]
    r.subst[r](c.subst[c](b.subst[b](a.subst[a](refl))))
  }

  /**
   * Lift subtyping into a function 
   */
  def liftF4[F[-_, -_, -_, -_, +_], A, A2, B, B2, C, C2, D, D2, R, R2](
     a: A <~< A2,
     b: B <~< B2,
     c: C <~< C2,
     d: D <~< D2,
     r: R <~< R2
   ): (F[A2, B2, C2, D2, R] <~< F[A, B, C, D, R2]) = {
    type a[-X] = F[A2, B2, C2, D2, R2] <~< F[X, B2, C2, D2, R2]
    type b[-X] = F[A2, B2, C2, D2, R2] <~< F[A, X, C2, D2, R2]
    type c[-X] = F[A2, B2, C2, D2, R2] <~< F[A, B, X, D2, R2]
    type d[-X] = F[A2, B2, C2, D2, R2] <~< F[A, B, C, X, R2]
    type r[-X] = F[A2, B2, C2, D2, X] <~< F[A, B, C, D, R2]
    r.subst[r](d.subst[d](c.subst[c](b.subst[b](a.subst[a](refl)))))
  }

  /**
   * Unsafely force a claim that A is a subtype of B 
   */
  def force[A, B]: A <~< B =
    new (A <~< B) {
      def subst[F[-_]](p: F[B]): F[A] = p.asInstanceOf[F[A]]
    }
}
