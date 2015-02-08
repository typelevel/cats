package cats
package std

trait EitherInstances extends EitherInstances0 {
  implicit def eitherMonadCombine[A: Monoid]: Monad[Either[A, ?]] with MonadCombine[Either[A, ?]] = new EitherMonad[A] with MonadCombine[Either[A, ?]] {
    def empty[B]: Either[A, B] = Left(Monoid[A].empty)
    def combine[B](x: Either[A, B], y: Either[A, B]): Either[A, B] = (x, y) match {
      case (Left(_), Right(_)) => y
      case _ => x
    }
  }

  implicit def eitherTraverse[A]: Traverse[Either[A, ?]] = new Traverse[Either[A, ?]] {
    def traverse[F[_]: Applicative, B, C](fa: Either[A, B])(f: B => F[C]): F[Either[A, C]] =
      fa.fold(
        a => Applicative[F].pure(Left(a)),
        b => Applicative[F].map(f(b))(Right(_))
      )

    def foldLeft[B, C](fa: Either[A, B], c: C)(f: (C, B) => C): C =
      fa.fold(_ => c, f(c, _))

    def foldRight[B, C](fa: Either[A, B], c: C)(f: (B, C) => C): C =
      fa.fold(_ => c, f(_, c))

    def foldRight[B, C](fa: Either[A, B], c: Lazy[C])(f: (B, Lazy[C]) => C): Lazy[C] =
      fa.fold(_ => c, b => Lazy(f(b, c)))
  }
}

trait EitherInstances0 extends EitherInstances1 {
  implicit def eitherInstances[A]: Monad[Either[A, ?]] = new EitherMonad[A] {}

  implicit def eitherOrder[A: Order, B: Order]: Order[Either[A, B]] = new Order[Either[A, B]] {
    def compare(x: Either[A, B], y: Either[A, B]): Int = x.fold(
      a => y.fold(Order[A].compare(a, _), _ => -1),
      b => y.fold(_ => 1, Order[B].compare(b, _))
    )
  }

  implicit def eitherShow[A, B](implicit AA: Show[A], BB: Show[B]): Show[Either[A, B]] =
    new Show[Either[A, B]] {
      def show(f: Either[A, B]): String = f.fold(
        a => s"Left(${AA.show(a)})",
        b => s"Right(${BB.show(b)})"
      )
    }
}

private trait EitherMonad[A] extends Monad[Either[A, ?]] {
  def pure[B](b: B): Either[A, B] = Right(b)

  def flatMap[B, C](fa: Either[A, B])(f: B => Either[A, C]): Either[A, C] =
    fa.right.flatMap(f)

  override def map[B, C](fa: Either[A, B])(f: B => C): Either[A, C] =
    fa.right.map(f)
}

sealed trait EitherInstances1 extends EitherInstances2 {
  implicit def eitherPartialOrder[A: PartialOrder, B: PartialOrder]: PartialOrder[Either[A, B]] =
    new PartialOrder[Either[A, B]] {
      def partialCompare(x: Either[A, B], y: Either[A, B]): Double = x.fold(
        a => y.fold(PartialOrder[A].partialCompare(a, _), _ => -1),
        b => y.fold(_ => 1, PartialOrder[B].partialCompare(b, _))
      )
    }
}

sealed trait EitherInstances2 {
  implicit def eitherEq[A: Eq, B: Eq]: Eq[Either[A, B]] = new Eq[Either[A, B]] {
    def eqv(x: Either[A, B], y: Either[A, B]): Boolean = x.fold(
      a => y.fold(Eq[A].eqv(a, _), _ => false),
      b => y.fold(_ => false, Eq[B].eqv(b, _))
    )
  }
}
