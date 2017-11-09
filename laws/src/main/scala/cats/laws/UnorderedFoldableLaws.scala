package cats
package laws

import cats.kernel.CommutativeMonoid

trait UnorderedFoldableLaws[F[_]] {
  def F: UnorderedFoldable[F]

  /**
    * If `F[A]` is empty, forall must return true.
    */
  def forallEmpty[A](fa: F[A], p: A => Boolean): Boolean = {
    !F.isEmpty(fa) || F.forall(fa)(p)
  }

  /**
    * If `F[A]` is empty, exists must return false.
    */
  def existsEmpty[A](fa: F[A], p: A => Boolean): Boolean = {
    !(F.isEmpty(fa) && F.exists(fa)(p))
  }

  /**
    * If `F[A]` is empty, its size must be zero;
    * if it is non-empty, its size must be positive.
    */
  def sizeEmpty[A](fa: F[A]): Boolean = {
    if (F.isEmpty(fa)) F.size(fa) == 0
    else F.size(fa) > 0
  }

  def forallConsistentWithExists[A](
    fa: F[A],
    p: A => Boolean
  ): Boolean = {
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

  def foldUnorderedIsFoldMapUnorderedWithIdentity[A: CommutativeMonoid](fa: F[A]): IsEq[A] =
    F.foldUnordered(fa) <-> F.foldMapUnordered(fa)(identity)

  def sequenceUnordered_IsTraverseUnordered_WithIdentity[G[_]: CommutativeApplicative, A](fga: F[G[A]]): IsEq[G[Unit]] =
    F.sequenceUnordered_(fga) <-> F.traverseUnordered_(fga)(identity)

  def existsLazy[A](fa: F[A]): Boolean = {
    var i = 0
    F.exists(fa){ _ =>
      i = i + 1
      true
    }
    i == (if (F.isEmpty(fa)) 0 else 1)
  }

  def forallLazy[A](fa: F[A]): Boolean = {
    var i = 0
    F.forall(fa){ _ =>
      i = i + 1
      false
    }
    i == (if (F.isEmpty(fa)) 0 else 1)
  }

  def existsConsistentWithFindAny[A](fa: F[A], p: A => Boolean): Boolean = {
    F.exists(fa)(p) == F.findAny(fa)(p).isDefined
  }

  def forallConsistentWithFindAny[A](fa: F[A], p: A => Boolean): Boolean = {
    F.forall(fa)(a => !p(a)) == F.findAny(fa)(p).isEmpty
  }

}

object UnorderedFoldableLaws {
  def apply[F[_]](implicit ev: UnorderedFoldable[F]): UnorderedFoldableLaws[F] =
    new UnorderedFoldableLaws[F] { def F: UnorderedFoldable[F] = ev }
}
