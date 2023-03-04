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

package cats

/**
 * Invariant version of a Monoidal.
 *
 * Must obey the laws defined in cats.laws.InvariantMonoidalLaws.
 */
trait InvariantMonoidal[F[_]] extends InvariantSemigroupal[F] {

  /**
   * `point` lifts any value into a Monoidal Functor.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> InvariantMonoidal[Option].point(10)
   * res0: Option[Int] = Some(10)
   * }}}
   */
  def point[A](a: A): F[A] = imap(unit)(_ => a)(_ => ())

  def unit: F[Unit]

}

object InvariantMonoidal {

  /**
   * Gives a `Monoid` instance if A itself has a `Monoid` instance.
   */
  def monoid[F[_], A](implicit F: InvariantMonoidal[F], A: Monoid[A]): Monoid[F[A]] =
    new InvariantMonoidalMonoid[F, A](F, A)

  /**
   * Summon an instance of [[InvariantMonoidal]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: InvariantMonoidal[F]): InvariantMonoidal[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllInvariantMonoidalOps[F[_], A](target: F[A])(implicit tc: InvariantMonoidal[F]): AllOps[F, A] {
      type TypeClassType = InvariantMonoidal[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = InvariantMonoidal[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: InvariantMonoidal[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with InvariantSemigroupal.AllOps[F, A] {
    type TypeClassType <: InvariantMonoidal[F]
  }
  trait ToInvariantMonoidalOps extends Serializable {
    implicit def toInvariantMonoidalOps[F[_], A](target: F[A])(implicit tc: InvariantMonoidal[F]): Ops[F, A] {
      type TypeClassType = InvariantMonoidal[F]
    } =
      new Ops[F, A] {
        type TypeClassType = InvariantMonoidal[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToInvariantMonoidalOps

}

private[cats] class InvariantMonoidalMonoid[F[_], A](f: InvariantMonoidal[F], monoid: Monoid[A])
    extends InvariantSemigroupalSemigroup(f, monoid)
    with Monoid[F[A]] {
  def empty: F[A] = f.point(monoid.empty)
}
