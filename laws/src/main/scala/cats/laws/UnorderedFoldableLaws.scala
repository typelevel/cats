package cats
package laws

import cats.kernel.CommutativeMonoid

trait UnorderedFoldableLaws[F[_]] {
  implicit def F: UnorderedFoldable[F]

  def unorderedFoldConsistentWithUnorderedFoldMap[A: CommutativeMonoid](fa: F[A]): IsEq[A] =
    F.unorderedFoldMap(fa)(identity) <-> F.unorderedFold(fa)



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

  /**
    * If `F[A]` is empty, forall must return true.
    */
  def forallEmpty[A](fa: F[A], p: A => Boolean): Boolean = {
    !F.isEmpty(fa) || F.forall(fa)(p)
  }


  def nonEmptyRef[A](fa: F[A]): IsEq[Boolean] =
    F.nonEmpty(fa) <-> !F.isEmpty(fa)

}

object UnorderedFoldableLaws {
  def apply[F[_]](implicit ev: UnorderedFoldable[F]): UnorderedFoldableLaws[F] =
    new UnorderedFoldableLaws[F] { def F: UnorderedFoldable[F] = ev }
}
