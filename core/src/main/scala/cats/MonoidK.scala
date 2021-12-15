package cats

import simulacrum.typeclass

/**
 * MonoidK is a universal monoid which operates on kinds.
 *
 * This type class is useful when its type parameter F[_] has a
 * structure that can be combined for any particular type, and which
 * also has an "empty" representation. Thus, MonoidK is like a Monoid
 * for kinds (i.e. parametrized types).
 *
 * A MonoidK[F] can produce a Monoid[F[A]] for any type A.
 *
 * Here's how to distinguish Monoid and MonoidK:
 *
 *  - Monoid[A] allows A values to be combined, and also means there
 *    is an "empty" A value that functions as an identity.
 *
 *  - MonoidK[F] allows two F[A] values to be combined, for any A.  It
 *    also means that for any A, there is an "empty" F[A] value. The
 *    combination operation and empty value just depend on the
 *    structure of F, but not on the structure of A.
 */
@typeclass trait MonoidK[F[_]] extends SemigroupK[F] { self =>

  /**
   * Given a type A, create an "empty" F[A] value.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> MonoidK[List].empty[Long]
   * res0: List[Long] = List()
   * }}}
   */
  def empty[A]: F[A]

  /**
   * Tests if `a` is the identity.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].isEmpty("")
   * res0: Boolean = true
   *
   * scala> Monoid[String].isEmpty("something")
   * res1: Boolean = false
   * }}}
   */
  def isEmpty[A](a: F[A])(implicit ev: Eq[F[A]]): Boolean =
    ev.eqv(a, empty)

  /**
   * Given a type A, create a concrete Monoid[F[A]].
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> MonoidK[List].algebra[Long].empty
   * res0: List[Long] = List()
   * }}}
   */
  override def algebra[A]: Monoid[F[A]] =
    new Monoid[F[A]] {
      def empty: F[A] = self.empty
      def combine(x: F[A], y: F[A]): F[A] = self.combineK(x, y)
    }

  /**
   * Given a kind G, create an "composed" MonoidK[F[G[_]]
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val monoidK = MonoidK[List].compose[Option]
   * scala> monoidK.combineK(List(Some(1)), List(Some(2), None))
   * res0: List[Option[Int]] = List(Some(1), Some(2), None)
   * }}}
   */
  override def compose[G[_]]: MonoidK[λ[α => F[G[α]]]] =
    new ComposedMonoidK[F, G] {
      val F: MonoidK[F] = self
    }

  /**
   * Return `a` appended to itself `n` times.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].combineN("ha", 3)
   * res0: String = hahaha
   *
   * scala> Monoid[String].combineN("ha", 0)
   * res1: String = ""
   * }}}
   */
  override def combineNK[A](a: F[A], n: Int): F[A] =
    if (n < 0) throw new IllegalArgumentException("Repeated combining for monoids must have n >= 0")
    else if (n == 0) empty[A]
    else repeatedCombineNK(a, n)

  /**
   * Given a sequence of `as`, sum them using the monoid and return the total.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].combineAll(List("One ", "Two ", "Three"))
   * res0: String = One Two Three
   *
   * scala> Monoid[String].combineAll(List.empty)
   * res1: String = ""
   * }}}
   */
  def combineAllK[A](as: IterableOnce[F[A]]): F[A] =
    as.iterator.foldLeft(empty[A])(combineK[A])

  override def combineAllOptionK[A](as: IterableOnce[F[A]]): Option[F[A]] =
    if (as.iterator.isEmpty) None else Some(combineAllK(as))

  override def reverse: MonoidK[F] =
    new MonoidK[F] {
      def empty[A] = self.empty
      def combineK[A](a: F[A], b: F[A]) = self.combineK(b, a)
      // a + a + a + ... is the same when reversed
      override def combineNK[A](a: F[A], n: Int): F[A] = self.combineNK(a, n)
      override def reverse = self
    }
}

object MonoidK {

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[MonoidK]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: MonoidK[F]): MonoidK[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllMonoidKOps[F[_], A](target: F[A])(implicit tc: MonoidK[F]): AllOps[F, A] {
      type TypeClassType = MonoidK[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = MonoidK[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: MonoidK[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with SemigroupK.AllOps[F, A] {
    type TypeClassType <: MonoidK[F]
  }
  trait ToMonoidKOps extends Serializable {
    implicit def toMonoidKOps[F[_], A](target: F[A])(implicit tc: MonoidK[F]): Ops[F, A] {
      type TypeClassType = MonoidK[F]
    } =
      new Ops[F, A] {
        type TypeClassType = MonoidK[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToMonoidKOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
