package cats
package compat

private[cats] trait FoldableCompat[F[_]] { this: Foldable[F] =>

  def iterable[A](fa: F[A]): Stream[A] =
    foldRight[A, Stream[A]](fa, Eval.now(Stream.empty)) { (a, eb) =>
      eb.map(Stream.cons(a, _))
    }.value
}

private[cats] object FoldableCompat {

  trait ToFoldableCompatOps {
    implicit final def foldableCompatSyntax[F[_], A](fa: F[A]): FoldableCompatAllOps[F, A] =
      new FoldableCompatAllOps(fa)
  }

  final class FoldableCompatAllOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def iterable(implicit F: Foldable[F]): Stream[A] = F.iterable(fa)
  }
}
