package cats

// TODO simulacrum bug? @typeclass doesn't work
trait Selective[F[_]]  {

  def applicative: Applicative[F]

  def select[A, B](fab: F[Either[A, B]])(fn: F[A => B]): F[B]

  def pure[A](a: A): F[A] = applicative.pure(a)

  def map[A, B](fa: F[A])(f: A => B): F[B] = applicative.map(fa)(f)

  def branch[A, B, C](x: F[Either[A, B]])(l: F[A => C])(r: F[B => C]): F[C] = {
    val lhs = {
      val innerLhs: F[Either[A, Either[B, C]]] = map(x)(_.map(Left(_)))
      val innerRhs: F[A => Either[B, C]] = map(l)(_.andThen(Right(_)))
      select(innerLhs)(innerRhs)
    }
    select(lhs)(r)
  }

  def ifS[A](x: F[Boolean])(t: F[A])(e: F[A]): F[A] =
    branch(map(x)(p => if (p) Left(()) else Right(())))(map(t)(a => (_: Unit) => a))(map(e)(a => _ => a))

  // TODO more combinators here

}


object Selective {

  def fromMonad[F[_]](implicit M: Monad[F]): Selective[F] =
    new Selective[F] {
      val applicative: Applicative[F] = M
      def select[A, B](fa: F[Either[A, B]])(fn: F[A => B]): F[B] =
        M.flatMap(fa) {
          case Right(b) => M.pure(b)
          case Left(a) => M.map(fn)(_(a))
        }
    }

}

