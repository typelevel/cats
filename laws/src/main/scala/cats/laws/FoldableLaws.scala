package cats
package laws

import cats.implicits._

trait FoldableLaws[F[_]] {
  implicit def F: Foldable[F]

  def leftFoldConsistentWithFoldMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit
    M: Monoid[B]
  ): IsEq[B] = {
    fa.foldMap(f) <-> fa.foldLeft(M.empty) { (b, a) => b |+| f(a) }
  }

  def rightFoldConsistentWithFoldMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit
    M: Monoid[B]
  ): IsEq[B] = {
    fa.foldMap(f) <-> fa.foldRight(Later(M.empty))((a, lb) => lb.map(f(a) |+| _)).value
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

  /**
   * If `F[A]` is empty, forall must return true.
   */
  def forallEmpty[A](
    fa: F[A],
    p: A => Boolean
  ): Boolean = {
    !F.isEmpty(fa) || F.forall(fa)(p)
  }

  /**
   * Monadic folding with identity monad is analogous to `foldLeft`.
   */
  def foldMIdentity[A, B](
    fa: F[A],
    b: B,
    f: (B, A) => B
  ): IsEq[B] = {
    F.foldM[Id, A, B](fa, b)(f) <-> F.foldLeft(fa, b)(f)
  }

  /**
   * `reduceLeftOption` consistent with `reduceLeftToOption`
   */
  def reduceLeftOptionConsistentWithReduceLeftToOption[A](
    fa: F[A],
    f: (A, A) => A
  ): IsEq[Option[A]] = {
    F.reduceLeftOption(fa)(f) <-> F.reduceLeftToOption(fa)(identity)(f)
  }

  /**
   * `reduceRightOption` consistent with `reduceRightToOption`
   */
  def reduceRightOptionConsistentWithReduceRightToOption[A](
    fa: F[A],
    f: (A, A) => A
  ): IsEq[Option[A]] = {
    val g: (A, Eval[A]) => Eval[A] = (a, ea) => ea.map(f(a, _))
    F.reduceRightOption(fa)(g).value <-> F.reduceRightToOption(fa)(identity)(g).value
  }
}

object FoldableLaws {
  def apply[F[_]](implicit ev: Foldable[F]): FoldableLaws[F] =
    new FoldableLaws[F] { def F: Foldable[F] = ev }
}
