package cats
package laws

trait FoldableLaws[F[_]] {
  implicit def F: Foldable[F]

  def leftFoldConsistentWithFoldMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit
    M: Monoid[B]
  ): IsEq[B] = {
    F.foldMap(fa)(f) <-> F.foldLeft(fa, M.empty){ (b, a) => M.combine(b, f(a)) }
  }

  def rightFoldConsistentWithFoldMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit
    M: Monoid[B]
  ): IsEq[B] = {
    F.foldMap(fa)(f) <-> F.foldRight(fa, Lazy(M.empty)){ a => Fold.Continue(M.combine(_, f(a)))}.value
  }

  def existsConsistentWithFind[A](
    fa: F[A],
    p: A => Boolean
  ): Boolean = {
    F.exists(fa)(p) == F.find(fa)(p).isDefined
  }

  def existsLazy[A](fa: F[A]): Boolean = {
    var i = 0
    F.exists(fa){ _ =>
      i = i + 1
      true
    }
    i == (if (F.empty(fa)) 0 else 1)
  }

  def forallLazy[A](fa: F[A]): Boolean = {
    var i = 0
    F.forall(fa){ _ =>
      i = i + 1
      false
    }
    i == (if (F.empty(fa)) 0 else 1)
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
        (F.empty(fa) || F.exists(fa)(p))
    } else true // can't test much in this case
  }

  /**
   * If `F[A]` is empty, forall must return true.
   */
  def forallEmpty[A](
    fa: F[A],
    p: A => Boolean
  ): Boolean = {
    !F.empty(fa) || F.forall(fa)(p)
  }
}

object FoldableLaws {
  def apply[F[_]](implicit ev: Foldable[F]): FoldableLaws[F] =
    new FoldableLaws[F] { def F: Foldable[F] = ev }
}
