package cats
package laws

import cats.implicits._
import cats.kernel.CommutativeMonoid

import scala.collection.mutable


trait UnorderedFoldableLaws[F[_]] {
  implicit def F: UnorderedFoldable[F]

  def foldLeftConsistentWithUnorderedFoldMap[A, B](fa: F[A], f: A => B)
                                         (implicit B: CommutativeMonoid[B]): IsEq[B] =
    F.unorderedFoldMap(fa)(f) <-> F.foldLeft(fa, B.empty) { (b, a) => b |+| f(a) }

  def unorderedFoldConsistentWithUnorderedFoldMap[A: CommutativeMonoid](fa: F[A]): IsEq[A] =
    F.unorderedFoldMap(fa)(identity) <-> F.unorderedFold(fa)

  def existsConsistentWithFind[A](fa: F[A], p: A => Boolean): Boolean = {
    F.exists(fa)(p) == F.find(fa)(p).isDefined
  }

  def forallConsistentWithExists[A](fa: F[A], p: A => Boolean): Boolean = {
    if (F.forall(fa)(p)) {
      val negationExists = F.exists(fa)(a => !(p(a)))

      // if p is true for all elements, then there cannot be an element for which
      // it does not hold.
      !negationExists &&
        // if p is true for all elements, then either there must be no elements
        // or there must exist an element for which it is true.
        (F.isEmpty(fa) || F.exists(fa)(p))
    } else true // can't test much in this case
  }

  /**
    * If `F[A]` is empty, forall must return true.
    */
  def forallEmpty[A](fa: F[A], p: A => Boolean): Boolean = {
    !F.isEmpty(fa) || F.forall(fa)(p)
  }

  def toSetRef[A](fa: F[A]): IsEq[Set[A]] =
    F.toSet(fa) <-> F.foldLeft(fa, mutable.ListBuffer.empty[A]) { (buf, a) =>
      buf += a
    }.toSet

  def nonEmptyRef[A](fa: F[A]): IsEq[Boolean] =
    F.nonEmpty(fa) <-> !F.isEmpty(fa)

}

object UnorderedFoldableLaws {
  def apply[F[_]](implicit ev: UnorderedFoldable[F]): UnorderedFoldableLaws[F] =
    new UnorderedFoldableLaws[F] { def F: UnorderedFoldable[F] = ev }
}
