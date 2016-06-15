package cats
package laws

import cats.instances.list._

trait UnfoldableLaws[F[_]] {
  implicit def F: Unfoldable[F]

  def noneConsistentWithDefault[A]: IsEq[F[A]] =
    F.none[A] <-> Unfoldable.DefaultImpl.none[F, A]

  def singletonConsistentWithDefault[A](a: A): IsEq[F[A]] =
    F.singleton(a) <-> Unfoldable.DefaultImpl.singleton[F, A](a)

  def replicateConsistentWithDefault[A](n: Int, a: A): IsEq[F[A]] =
    F.replicate(n)(a) <-> Unfoldable.DefaultImpl.replicate[F, A](n)(a)

  def buildConsistentWithDefault[A](as: List[A]): IsEq[F[A]] =
    F.build(as: _*) <-> Unfoldable.DefaultImpl.build[F, A](as: _*)

  def fromFoldableConsistentWithDefault[A](as: List[A]): IsEq[F[A]] =
    F.fromFoldable(as) <-> Unfoldable.DefaultImpl.fromFoldable[F, List, A](as)
}

object UnfoldableLaws {
  def apply[F[_]](implicit ev: Unfoldable[F]): UnfoldableLaws[F] =
    new UnfoldableLaws[F] { def F: Unfoldable[F] = ev }
}
