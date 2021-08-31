package cats
package compat

private[cats] object FoldableCompat {

  def toIterable[F[_], A](fa: F[A])(F: Foldable[F]): Iterable[A] =
    F.foldRight[A, LazyList[A]](fa, Eval.now(LazyList.empty)) { (a, eb) =>
      eb.map(LazyList.cons(a, _))
    }.value
}
