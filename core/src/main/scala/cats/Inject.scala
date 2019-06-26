package cats

import kernel.compat.scalaVersionMoreSpecific._

/**
 * Inject is a type class providing an injection from type `A` into
 * type `B`. An injection is a function `inj` which does not destroy
 * any information: for every `b: B` there is at most one `a: A` such
 * that `inj(a) = b`.
 *
 * Because of this all injections admit partial inverses `prj` which
 * pair a value `b: B` back with a single value `a: A`.
 *
 * @since 1.0
 * @note Prior to cats 1.0, Inject handled injection for type
 * constructors. For injection of type constructors, use [[InjectK]].
 *
 * @see [[InjectK]] for injection for [[cats.data.EitherK]]
 */
abstract class Inject[A, B] {
  def inj: A => B

  def prj: B => Option[A]

  final def apply(a: A): B = inj(a)

  final def unapply(b: B): Option[A] = prj(b)
}

@suppressUnusedImportWarningForScalaVersionMoreSpecific
sealed abstract private[cats] class InjectInstances {
  implicit def catsReflexiveInjectInstance[A]: Inject[A, A] =
    new Inject[A, A] {
      val inj = identity(_: A)

      val prj = Some(_: A)
    }

  implicit def catsLeftInjectInstance[A, B]: Inject[A, Either[A, B]] =
    new Inject[A, Either[A, B]] {
      val inj = Left(_: A)

      val prj = (_: Either[A, B]).left.toOption
    }

  implicit def catsRightInjectInstance[A, B, C](implicit I: Inject[A, B]): Inject[A, Either[C, B]] =
    new Inject[A, Either[C, B]] {
      val inj = (a: A) => Right(I.inj(a))

      val prj = (_: Either[C, B]).toOption.flatMap(I.prj)
    }

}

object Inject extends InjectInstances {
  def apply[A, B](implicit I: Inject[A, B]): Inject[A, B] = I
}
