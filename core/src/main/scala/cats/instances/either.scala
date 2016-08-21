package cats
package instances

import cats.data.Xor
import cats.syntax.EitherUtil
import cats.syntax.either._
import scala.annotation.tailrec

trait EitherInstances extends EitherInstances1 {
  implicit val catsStdBitraverseForEither: Bitraverse[Either] =
    new Bitraverse[Either] {
      def bitraverse[G[_], A, B, C, D](fab: Either[A, B])(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[Either[C, D]] =
        fab match {
          case Left(a) => G.map(f(a))(Left(_))
          case Right(b) => G.map(g(b))(Right(_))
        }

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

  // scalastyle:off method.length
  implicit def catsStdInstancesForEither[A]: MonadError[Either[A, ?], A] with Traverse[Either[A, ?]] with RecursiveTailRecM[Either[A, ?]] =
    new MonadError[Either[A, ?], A] with Traverse[Either[A, ?]] with RecursiveTailRecM[Either[A, ?]] {
      def pure[B](b: B): Either[A, B] = Right(b)

      def flatMap[B, C](fa: Either[A, B])(f: B => Either[A, C]): Either[A, C] =
        fa.right.flatMap(f)

      def handleErrorWith[B](fea: Either[A, B])(f: A => Either[A, B]): Either[A, B] =
        fea match {
          case Left(e) => f(e)
          case r @ Right(_) => r
        }
      def raiseError[B](e: A): Either[A, B] = Left(e)

      override def map[B, C](fa: Either[A, B])(f: B => C): Either[A, C] =
        fa.right.map(f)

      @tailrec
      def tailRecM[B, C](b: B)(f: B => Either[A, Either[B, C]]): Either[A, C] =
        f(b) match {
          case Left(a)         => Left(a)
          case Right(Left(b1)) => tailRecM(b1)(f)
          case Right(Right(c)) => Right(c)
        }

      override def map2Eval[B, C, Z](fb: Either[A, B], fc: Eval[Either[A, C]])(f: (B, C) => Z): Eval[Either[A, Z]] =
        fb match {
          case l @ Left(_) => Now(EitherUtil.rightCast(l))
          case Right(b) => fc.map(_.right.map(f(b, _)))
        }

      def traverse[F[_], B, C](fa: Either[A, B])(f: B => F[C])(implicit F: Applicative[F]): F[Either[A, C]] =
        fa.fold(
          a => F.pure(Left(a)),
          b => F.map(f(b))(Right(_))
        )

      def foldLeft[B, C](fa: Either[A, B], c: C)(f: (C, B) => C): C =
        fa.fold(_ => c, f(c, _))

      def foldRight[B, C](fa: Either[A, B], lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
        fa.fold(_ => lc, b => f(b, lc))

      override def attempt[B](fab: Either[A, B]): Either[A, Xor[A, B]] = Right(fab.toXor)
      override def recover[B](fab: Either[A, B])(pf: PartialFunction[A, B]): Either[A, B] =
        fab recover pf
      override def recoverWith[B](fab: Either[A, B])(pf: PartialFunction[A, Either[A, B]]): Either[A, B] =
        fab recoverWith pf
      override def ensure[B](fab: Either[A, B])(error: => A)(predicate: B => Boolean): Either[A, B] =
        fab.ensure(error)(predicate)
    }
  // scalastyle:on method.length

  implicit def catsStdOrderForEither[A, B](implicit A: Order[A], B: Order[B]): Order[Either[A, B]] = new Order[Either[A, B]] {
    def compare(x: Either[A, B], y: Either[A, B]): Int = x.fold(
      a => y.fold(A.compare(a, _), _ => -1),
      b => y.fold(_ => 1, B.compare(b, _))
    )
  }

  implicit def catsStdShowForEither[A, B](implicit A: Show[A], B: Show[B]): Show[Either[A, B]] =
    new Show[Either[A, B]] {
      def show(f: Either[A, B]): String = f.fold(
        a => s"Left(${A.show(a)})",
        b => s"Right(${B.show(b)})"
      )
    }

  implicit def catsDataMonoidForEither[A, B](implicit B: Monoid[B]): Monoid[Either[A, B]] =
    new Monoid[Either[A, B]] {
      def empty: Either[A, B] = Right(B.empty)
      def combine(x: Either[A, B], y: Either[A, B]): Either[A, B] = x combine y
    }

  implicit def catsDataSemigroupKForEither[L]: SemigroupK[Either[L, ?]] =
    new SemigroupK[Either[L, ?]] {
      def combineK[A](x: Either[L, A], y: Either[L, A]): Either[L, A] = x match {
        case Left(_) => y
        case Right(_) => x
      }
    }
}

private[instances] sealed trait EitherInstances1 extends EitherInstances2 {
  implicit def catsStdPartialOrderForEither[A, B](implicit A: PartialOrder[A], B: PartialOrder[B]): PartialOrder[Either[A, B]] =
    new PartialOrder[Either[A, B]] {
      def partialCompare(x: Either[A, B], y: Either[A, B]): Double = x.fold(
        a => y.fold(A.partialCompare(a, _), _ => -1),
        b => y.fold(_ => 1, B.partialCompare(b, _))
      )
    }
}

private[instances] sealed trait EitherInstances2 {
  implicit def catsStdEqForEither[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Either[A, B]] = new Eq[Either[A, B]] {
    def eqv(x: Either[A, B], y: Either[A, B]): Boolean = x.fold(
      a => y.fold(A.eqv(a, _), _ => false),
      b => y.fold(_ => false, B.eqv(b, _))
    )
  }
}
