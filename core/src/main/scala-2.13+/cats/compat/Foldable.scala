package cats
package compat

private[cats] object Foldable {

  def iterator[F[_], A](fa: F[A])(F: Foldable[F]): Iterator[A] =
    F.foldRight[A, LazyList[A]](fa, Eval.now(LazyList.empty)) { (a, eb) =>
        eb.map(LazyList.cons(a, _))
      }
      .value
      .iterator
}
