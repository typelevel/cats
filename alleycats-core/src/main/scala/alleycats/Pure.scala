package alleycats

import cats.{Applicative, FlatMap, Monad}
import simulacrum.typeclass

@typeclass trait Pure[F[_]] {
  def pure[A](a: A): F[A]
}

object Pure {
  // Ideally this would be an exported subclass instance provided by Applicative
  implicit def applicativeIsPure[F[_]](implicit ev: Applicative[F]): Pure[F] =
    new Pure[F] {
      def pure[A](a: A): F[A] = ev.pure(a)
    }

  // Ideally this would be an instance exported to Monad
  implicit def pureFlatMapIsMonad[F[_]](implicit p: Pure[F], fm: FlatMap[F]): Monad[F] =
    new Monad[F] {
      def pure[A](a: A): F[A] = p.pure(a)
      override def map[A, B](fa: F[A])(f: A => B): F[B] = fm.map(fa)(f)
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = fm.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: (A) => F[Either[A, B]]): F[B] = fm.tailRecM(a)(f)
    }
}
