package cats

import simulacrum.{noop, typeclass}
import cats.data.Ior
import scala.annotation.implicitNotFound

/**
 * Weaker version of Applicative[F]; has apply but not pure.
 *
 * Must obey the laws defined in cats.laws.ApplyLaws.
 */
@implicitNotFound("Could not find an instance of Apply for ${F}")
@typeclass(excludeParents = List("ApplyArityFunctions"))
trait Apply[F[_]] extends Functor[F] with InvariantSemigroupal[F] with ApplyArityFunctions[F] { self =>

  /**
   * Given a value and a function in the Apply context, applies the
   * function to the value.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val someF: Option[Int => Long] = Some(_.toLong + 1L)
   * scala> val noneF: Option[Int => Long] = None
   * scala> val someInt: Option[Int] = Some(3)
   * scala> val noneInt: Option[Int] = None
   *
   * scala> Apply[Option].ap(someF)(someInt)
   * res0: Option[Long] = Some(4)
   *
   * scala> Apply[Option].ap(noneF)(someInt)
   * res1: Option[Long] = None
   *
   * scala> Apply[Option].ap(someF)(noneInt)
   * res2: Option[Long] = None
   *
   * scala> Apply[Option].ap(noneF)(noneInt)
   * res3: Option[Long] = None
   * }}}
   */
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  /**
   * Compose two actions, discarding any value produced by the first.
   *
   * @see [[productL]] to discard the value of the second instead.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.data.Validated
   * scala> import Validated.{Valid, Invalid}
   *
   * scala> type ErrOr[A] = Validated[String, A]
   *
   * scala> val validInt: ErrOr[Int] = Valid(3)
   * scala> val validBool: ErrOr[Boolean] = Valid(true)
   * scala> val invalidInt: ErrOr[Int] = Invalid("Invalid int.")
   * scala> val invalidBool: ErrOr[Boolean] = Invalid("Invalid boolean.")
   *
   * scala> Apply[ErrOr].productR(validInt)(validBool)
   * res0: ErrOr[Boolean] = Valid(true)
   *
   * scala> Apply[ErrOr].productR(invalidInt)(validBool)
   * res1: ErrOr[Boolean] = Invalid(Invalid int.)
   *
   * scala> Apply[ErrOr].productR(validInt)(invalidBool)
   * res2: ErrOr[Boolean] = Invalid(Invalid boolean.)
   *
   * scala> Apply[ErrOr].productR(invalidInt)(invalidBool)
   * res3: ErrOr[Boolean] = Invalid(Invalid int.Invalid boolean.)
   * }}}
   */
  def productR[A, B](fa: F[A])(fb: F[B]): F[B] =
    ap(as(fa, { (b: B) => b }))(fb)

  /**
   * Compose two actions, discarding any value produced by the second.
   *
   * @see [[productR]] to discard the value of the first instead.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.data.Validated
   * scala> import Validated.{Valid, Invalid}
   *
   * scala> type ErrOr[A] = Validated[String, A]
   *
   * scala> val validInt: ErrOr[Int] = Valid(3)
   * scala> val validBool: ErrOr[Boolean] = Valid(true)
   * scala> val invalidInt: ErrOr[Int] = Invalid("Invalid int.")
   * scala> val invalidBool: ErrOr[Boolean] = Invalid("Invalid boolean.")
   *
   * scala> Apply[ErrOr].productL(validInt)(validBool)
   * res0: ErrOr[Int] = Valid(3)
   *
   * scala> Apply[ErrOr].productL(invalidInt)(validBool)
   * res1: ErrOr[Int] = Invalid(Invalid int.)
   *
   * scala> Apply[ErrOr].productL(validInt)(invalidBool)
   * res2: ErrOr[Int] = Invalid(Invalid boolean.)
   *
   * scala> Apply[ErrOr].productL(invalidInt)(invalidBool)
   * res3: ErrOr[Int] = Invalid(Invalid int.Invalid boolean.)
   * }}}
   */
  def productL[A, B](fa: F[A])(fb: F[B]): F[A] =
    map2(fa, fb)((a, _) => a)

  override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(map(fa)(a => (b: B) => (a, b)))(fb)

  /**
   * Alias for [[ap]].
   */
  @inline final def <*>[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    ap(ff)(fa)

  /**
   * Alias for [[productR]].
   */
  @inline final def *>[A, B](fa: F[A])(fb: F[B]): F[B] =
    productR(fa)(fb)

  /**
   * Alias for [[productL]].
   */
  @inline final def <*[A, B](fa: F[A])(fb: F[B]): F[A] =
    productL(fa)(fb)

  /**
   * Alias for [[productR]].
   */
  @deprecated("Use *> or productR instead.", "1.0.0-RC2")
  @noop @inline final private[cats] def followedBy[A, B](fa: F[A])(fb: F[B]): F[B] =
    productR(fa)(fb)

  /**
   * Alias for [[productL]].
   */
  @deprecated("Use <* or productL instead.", "1.0.0-RC2")
  @noop @inline final private[cats] def forEffect[A, B](fa: F[A])(fb: F[B]): F[A] =
    productL(fa)(fb)

  /**
   * ap2 is a binary version of ap, defined in terms of ap.
   */
  def ap2[A, B, Z](ff: F[(A, B) => Z])(fa: F[A], fb: F[B]): F[Z] =
    map(product(fa, product(fb, ff))) { case (a, (b, f)) => f(a, b) }

  /**
   * Applies the pure (binary) function f to the effectful values fa and fb.
   *
   * map2 can be seen as a binary version of [[cats.Functor]]#map.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val someInt: Option[Int] = Some(3)
   * scala> val noneInt: Option[Int] = None
   * scala> val someLong: Option[Long] = Some(4L)
   * scala> val noneLong: Option[Long] = None
   *
   * scala> Apply[Option].map2(someInt, someLong)((i, l) => i.toString + l.toString)
   * res0: Option[String] = Some(34)
   *
   * scala> Apply[Option].map2(someInt, noneLong)((i, l) => i.toString + l.toString)
   * res0: Option[String] = None
   *
   * scala> Apply[Option].map2(noneInt, noneLong)((i, l) => i.toString + l.toString)
   * res0: Option[String] = None
   *
   * scala> Apply[Option].map2(noneInt, someLong)((i, l) => i.toString + l.toString)
   * res0: Option[String] = None
   * }}}
   */
  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    map(product(fa, fb))(f.tupled)

  /**
   * Similar to [[map2]] but uses [[Eval]] to allow for laziness in the `F[B]`
   * argument. This can allow for "short-circuiting" of computations.
   *
   * NOTE: the default implementation of `map2Eval` does not short-circuit
   * computations. For data structures that can benefit from laziness, [[Apply]]
   * instances should override this method.
   *
   * In the following example, `x.map2(bomb)(_ + _)` would result in an error,
   * but `map2Eval` "short-circuits" the computation. `x` is `None` and thus the
   * result of `bomb` doesn't even need to be evaluated in order to determine
   * that the result of `map2Eval` should be `None`.
   *
   * {{{
   * scala> import cats.{Eval, Later}
   * scala> import cats.implicits._
   * scala> val bomb: Eval[Option[Int]] = Later(sys.error("boom"))
   * scala> val x: Option[Int] = None
   * scala> x.map2Eval(bomb)(_ + _).value
   * res0: Option[Int] = None
   * }}}
   */
  def map2Eval[A, B, Z](fa: F[A], fb: Eval[F[B]])(f: (A, B) => Z): Eval[F[Z]] =
    fb.map(fb => map2(fa, fb)(f))

  /**
   * Compose an `Apply[F]` and an `Apply[G]` into an `Apply[λ[α => F[G[α]]]]`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val alo = Apply[List].compose[Option]
   *
   * scala> alo.product(List(None, Some(true), Some(false)), List(Some(2), None))
   * res1: List[Option[(Boolean, Int)]] = List(None, None, Some((true,2)), None, Some((false,2)), None)
   * }}}
   */
  def compose[G[_]: Apply]: Apply[λ[α => F[G[α]]]] =
    new ComposedApply[F, G] {
      val F = self
      val G = Apply[G]
    }

  @noop
  @deprecated("Dangerous method, use ifM (a flatMap) or ifF (a map) instead", "2.6.2")
  def ifA[A](fcond: F[Boolean])(ifTrue: F[A], ifFalse: F[A]): F[A] = {
    def ite(b: Boolean)(ifTrue: A, ifFalse: A) = if (b) ifTrue else ifFalse
    ap2(map(fcond)(ite))(ifTrue, ifFalse)
  }

  def traverseStrategy: Apply.TraverseStrategy[F] =
    Apply.TraverseStrategy.viaEval(this)
}

object Apply {

  /**
   * This type can be used by those implementing traverse-like
   * functions where you want to respect the potential laziness
   * or short-circuiting built into F if it exists without
   * having to pay an additional cost to wrap in Eval[F[A]]
   */
  trait TraverseStrategy[F[_]] extends Serializable { self =>
    type Rhs[_]

    def map2[A, B, C](left: Rhs[A], right: Rhs[B])(fn: (A, B) => C): Rhs[C]
    def applyToRhs[A, B](fn: A => F[B], arg: A): Rhs[B]
    def applyOnRhs[A, B](fn: A => Rhs[B], arg: A): Rhs[B]
    def rhsToF[A](r: Rhs[A]): F[A]
    def mapRhs[A, B](r: Rhs[A])(fn: A => B): Rhs[B]
  }

  object TraverseStrategy {
    final case class Direct[F[_]](F: Apply[F]) extends TraverseStrategy[F] {
      type Rhs[A] = F[A]
      def map2[A, B, C](left: Rhs[A], right: Rhs[B])(fn: (A, B) => C): Rhs[C] =
        F.map2(left, right)(fn)

      def applyToRhs[A, B](fn: A => F[B], arg: A): Rhs[B] = fn(arg)
      def applyOnRhs[A, B](fn: A => Rhs[B], arg: A): Rhs[B] = fn(arg)
      def rhsToF[A](r: Rhs[A]): F[A] = r
      def mapRhs[A, B](r: Rhs[A])(fn: A => B): Rhs[B] = F.map(r)(fn)
    }

    final case class ViaEval[F[_]](F: Apply[F]) extends TraverseStrategy[F] {
      type Rhs[A] = Eval[F[A]]
      def map2[A, B, C](left: Rhs[A], right: Rhs[B])(fn: (A, B) => C): Rhs[C] =
        left.flatMap { fa =>
          F.map2Eval(fa, right)(fn)
        }

      def applyToRhs[A, B](fn: A => F[B], arg: A): Rhs[B] = Eval.always(fn(arg))
      def applyOnRhs[A, B](fn: A => Rhs[B], arg: A): Rhs[B] = Eval.defer(fn(arg))
      def rhsToF[A](r: Rhs[A]): F[A] = r.value
      def mapRhs[A, B](r: Rhs[A])(fn: A => B): Rhs[B] = r.map { fa => F.map(fa)(fn) }
    }

    /**
     * This strategy directly does map2 on the type F.
     *
     * This is useful for Fs that are already lazy, or
     * have no short-circuiting properties
     */
    def direct[F[_]](a: Apply[F]): TraverseStrategy[F] =
      Direct(a)

    /**
     * This strategy wraps F[A] as Eval[F[A]] which
     * allows laziness in traverse if F can possibly stop early.
     * This is useful for strict error types (e.g. Either, Option)
     * or possibly empty collections which are similar to strict error types
     */
    def viaEval[F[_]](a: Apply[F]): TraverseStrategy[F] =
      ViaEval(a)

    def composeOuter[F[_], G[_]](self: TraverseStrategy[F], that: Apply[G]): TraverseStrategy[Lambda[x => F[G[x]]]] =
      new TraverseStrategy[Lambda[x => F[G[x]]]] {
        type Rhs[A] = self.Rhs[G[A]]

        def map2[A, B, C](left: Rhs[A], right: Rhs[B])(fn: (A, B) => C): Rhs[C] =
          self.map2(left, right) { (ga, gb) => that.map2(ga, gb)(fn) }
        def applyToRhs[A, B](fn: A => F[G[B]], arg: A): Rhs[B] =
          self.applyToRhs(fn, arg)

        def applyOnRhs[A, B](fn: A => Rhs[B], arg: A): Rhs[B] =
          self.applyOnRhs(fn, arg)

        def rhsToF[A](r: Rhs[A]): F[G[A]] =
          self.rhsToF(r)
        def mapRhs[A, B](r: Rhs[A])(fn: A => B): Rhs[B] =
          self.mapRhs(r) { ga => that.map(ga)(fn) }
      }
  }

  /**
   * This semigroup uses a product operation to combine `F`s.
   * If the `Apply[F].product` results in larger `F` (i.e. when `F` is a `List`),
   * accumulative usage of this instance, such as `combineAll`, will result in
   * `F`s with exponentially increasing sizes.
   */
  def semigroup[F[_], A](implicit f: Apply[F], sg: Semigroup[A]): Semigroup[F[A]] =
    new ApplySemigroup[F, A](f, sg)

  def align[F[_]: Apply]: Align[F] =
    new Align[F] {
      def align[A, B](fa: F[A], fb: F[B]): F[Ior[A, B]] = Apply[F].map2(fa, fb)(Ior.both)
      def functor: Functor[F] = Apply[F]
    }

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[Apply]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Apply[F]): Apply[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllApplyOps[F[_], A](target: F[A])(implicit tc: Apply[F]): AllOps[F, A] {
      type TypeClassType = Apply[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Apply[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Apply[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def ap[B, C](fa: F[B])(implicit ev$1: A <:< (B => C)): F[C] =
      typeClassInstance.ap[B, C](self.asInstanceOf[F[B => C]])(fa)
    def productR[B](fb: F[B]): F[B] = typeClassInstance.productR[A, B](self)(fb)
    def productL[B](fb: F[B]): F[A] = typeClassInstance.productL[A, B](self)(fb)
    @inline final def <*>[B, C](fa: F[B])(implicit ev$1: A <:< (B => C)): F[C] =
      typeClassInstance.<*>[B, C](self.asInstanceOf[F[B => C]])(fa)
    @inline final def *>[B](fb: F[B]): F[B] = typeClassInstance.*>[A, B](self)(fb)
    @inline final def <*[B](fb: F[B]): F[A] = typeClassInstance.<*[A, B](self)(fb)
    def ap2[B, C, D](fa: F[B], fb: F[C])(implicit ev$1: A <:< ((B, C) => D)): F[D] =
      typeClassInstance.ap2[B, C, D](self.asInstanceOf[F[(B, C) => D]])(fa, fb)
    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] = typeClassInstance.map2[A, B, C](self, fb)(f)
    def map2Eval[B, C](fb: Eval[F[B]])(f: (A, B) => C): Eval[F[C]] = typeClassInstance.map2Eval[A, B, C](self, fb)(f)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Functor.AllOps[F, A] with InvariantSemigroupal.AllOps[F, A] {
    type TypeClassType <: Apply[F]
  }
  trait ToApplyOps extends Serializable {
    implicit def toApplyOps[F[_], A](target: F[A])(implicit tc: Apply[F]): Ops[F, A] {
      type TypeClassType = Apply[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Apply[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToApplyOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}

private[cats] class ApplySemigroup[F[_], A](f: Apply[F], sg: Semigroup[A]) extends Semigroup[F[A]] {
  def combine(a: F[A], b: F[A]): F[A] =
    f.map2(a, b)(sg.combine)
}
