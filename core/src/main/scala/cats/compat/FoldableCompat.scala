package cats.compat

import cats.{Eval, Foldable}

private[cats] object FoldableCompat {

  def toIterable[F[_], A](fa: F[A])(F: Foldable[F]): Iterable[A] =
    F.foldRight[A, Stream[A]](fa, Eval.now(Stream.empty)) { (a, eb) =>
        eb.map(Stream.cons(a, _))
      }
      .value
}
