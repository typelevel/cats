package cats
package std

trait EitherInstances extends EitherInstances1 {
  implicit def eitherInstances[A]: Monad[Either[A, ?]] with Traverse[Either[A, ?]] =
    new Monad[Either[A, ?]] with Traverse[Either[A, ?]] {
      def pure[B](b: B): Either[A, B] = Right(b)

      def flatMap[B, C](fa: Either[A, B])(f: B => Either[A, C]): Either[A, C] =
        fa.right.flatMap(f)

      override def map[B, C](fa: Either[A, B])(f: B => C): Either[A, C] =
        fa.right.map(f)

      def traverse[F[_], B, C](fa: Either[A, B])(f: B => F[C])(implicit F: Applicative[F]): F[Either[A, C]] =
        fa.fold(
          a => F.pure(Left(a)),
          b => F.map(f(b))(Right(_))
        )

      def foldLeft[B, C](fa: Either[A, B], c: C)(f: (C, B) => C): C =
        fa.fold(_ => c, f(c, _))

      def partialFold[B, C](fa: Either[A, B])(f: B => Fold[C]): Fold[C] =
        fa.fold(_ => Fold.Pass, f)
    }

  implicit def eitherOrder[A, B](implicit A: Order[A], B: Order[B]): Order[Either[A, B]] = new Order[Either[A, B]] {
    def compare(x: Either[A, B], y: Either[A, B]): Int = x.fold(
      a => y.fold(A.compare(a, _), _ => -1),
      b => y.fold(_ => 1, B.compare(b, _))
    )
  }

  implicit def eitherShow[A, B](implicit A: Show[A], B: Show[B]): Show[Either[A, B]] =
    new Show[Either[A, B]] {
      def show(f: Either[A, B]): String = f.fold(
        a => s"Left(${A.show(a)})",
        b => s"Right(${B.show(b)})"
      )
    }
}

sealed trait EitherInstances1 extends EitherInstances2 {
  implicit def eitherPartialOrder[A, B](implicit A: PartialOrder[A], B: PartialOrder[B]): PartialOrder[Either[A, B]] =
    new PartialOrder[Either[A, B]] {
      def partialCompare(x: Either[A, B], y: Either[A, B]): Double = x.fold(
        a => y.fold(A.partialCompare(a, _), _ => -1),
        b => y.fold(_ => 1, B.partialCompare(b, _))
      )
    }
}

sealed trait EitherInstances2 {
  implicit def eitherEq[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Either[A, B]] = new Eq[Either[A, B]] {
    def eqv(x: Either[A, B], y: Either[A, B]): Boolean = x.fold(
      a => y.fold(A.eqv(a, _), _ => false),
      b => y.fold(_ => false, B.eqv(b, _))
    )
  }
}
