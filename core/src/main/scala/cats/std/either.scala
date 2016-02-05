package cats
package std

trait EitherInstances extends EitherInstances1 {
  implicit val eitherBifoldable: Bifoldable[Either] =
    new Bifoldable[Either] {
      def bifoldLeft[A, B, C](fab: Either[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
        fab match {
          case Left(a) => f(c, a)
          case Right(b) => g(c, b)
        }
      def bifoldRight[A, B, C](fab: Either[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        fab match {
          case Left(a) => f(a, c)
          case Right(b) => g(b, c)
        }
    }

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

      def foldRight[B, C](fa: Either[A, B], lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
        fa.fold(_ => lc, b => f(b, lc))
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

private[std] sealed trait EitherInstances1 extends EitherInstances2 {
  implicit def eitherPartialOrder[A, B](implicit A: PartialOrder[A], B: PartialOrder[B]): PartialOrder[Either[A, B]] =
    new PartialOrder[Either[A, B]] {
      def partialCompare(x: Either[A, B], y: Either[A, B]): Double = x.fold(
        a => y.fold(A.partialCompare(a, _), _ => -1),
        b => y.fold(_ => 1, B.partialCompare(b, _))
      )
    }
}

private[std] sealed trait EitherInstances2 {
  implicit def eitherEq[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Either[A, B]] = new Eq[Either[A, B]] {
    def eqv(x: Either[A, B], y: Either[A, B]): Boolean = x.fold(
      a => y.fold(A.eqv(a, _), _ => false),
      b => y.fold(_ => false, B.eqv(b, _))
    )
  }
}
