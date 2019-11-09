package cats
package compat

private[cats] trait FoldableCompat[F[_]] { this: Foldable[F] =>

  def iterable[A](fa: F[A]): LazyList[A] =
    foldRight[A, LazyList[A]](fa, Eval.now(LazyList.empty)) { (a, eb) =>
      eb.map(LazyList.cons(a, _))
    }.value
}
