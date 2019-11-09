package cats
package instances

import cats.data.Validated
import cats.kernel.Semigroup
import cats.syntax.EitherUtil
import cats.syntax.either._

import scala.annotation.tailrec
import cats.data.Ior

trait EitherInstances extends cats.kernel.instances.EitherInstances {
  implicit val catsStdBitraverseForEither: Bitraverse[Either] =
    new Bitraverse[Either] {
      def bitraverse[G[_], A, B, C, D](fab: Either[A, B])(f: A => G[C],
                                                          g: B => G[D])(implicit G: Applicative[G]): G[Either[C, D]] =
        fab match {
          case Left(a)  => G.map(f(a))(Left(_))
          case Right(b) => G.map(g(b))(Right(_))
        }

      def bifoldLeft[A, B, C](fab: Either[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
        fab match {
          case Left(a)  => f(c, a)
          case Right(b) => g(c, b)
        }

      def bifoldRight[A, B, C](fab: Either[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C],
                                                              g: (B, Eval[C]) => Eval[C]): Eval[C] =
        fab match {
          case Left(a)  => f(a, c)
          case Right(b) => g(b, c)
        }
    }

  // scalastyle:off method.length
  implicit def catsStdInstancesForEither[A]
    : MonadError[Either[A, *], A] with Traverse[Either[A, *]] with Align[Either[A, *]] =
    new MonadError[Either[A, *], A] with Traverse[Either[A, *]] with Align[Either[A, *]] {
      def pure[B](b: B): Either[A, B] = Right(b)

      def flatMap[B, C](fa: Either[A, B])(f: B => Either[A, C]): Either[A, C] =
        fa.flatMap(f)

      def handleErrorWith[B](fea: Either[A, B])(f: A => Either[A, B]): Either[A, B] =
        fea match {
          case Left(e)      => f(e)
          case r @ Right(_) => r
        }

      def raiseError[B](e: A): Either[A, B] = Left(e)

      override def map[B, C](fa: Either[A, B])(f: B => C): Either[A, C] =
        fa.map(f)

      @tailrec
      def tailRecM[B, C](b: B)(f: B => Either[A, Either[B, C]]): Either[A, C] =
        f(b) match {
          case left @ Left(_) =>
            left.rightCast[C]
          case Right(e) =>
            e match {
              case Left(b1)         => tailRecM(b1)(f)
              case right @ Right(_) => right.leftCast[A]
            }
        }

      override def map2Eval[B, C, Z](fb: Either[A, B], fc: Eval[Either[A, C]])(f: (B, C) => Z): Eval[Either[A, Z]] =
        fb match {
          case l @ Left(_) => Now(EitherUtil.rightCast(l))
          case Right(b)    => fc.map(_.map(f(b, _)))
        }

      def traverse[F[_], B, C](fa: Either[A, B])(f: B => F[C])(implicit F: Applicative[F]): F[Either[A, C]] =
        fa match {
          case left @ Left(_) => F.pure(left.rightCast[C])
          case Right(b)       => F.map(f(b))(Right(_))
        }

      def foldLeft[B, C](fa: Either[A, B], c: C)(f: (C, B) => C): C =
        fa match {
          case Left(_)  => c
          case Right(b) => f(c, b)
        }

      def foldRight[B, C](fa: Either[A, B], lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
        fa match {
          case Left(_)  => lc
          case Right(b) => f(b, lc)
        }

      override def attempt[B](fab: Either[A, B]): Either[A, Either[A, B]] =
        Right(fab)

      override def recover[B](fab: Either[A, B])(pf: PartialFunction[A, B]): Either[A, B] =
        fab.recover(pf)

      override def recoverWith[B](fab: Either[A, B])(pf: PartialFunction[A, Either[A, B]]): Either[A, B] =
        fab.recoverWith(pf)

      override def fromEither[B](fab: Either[A, B]): Either[A, B] =
        fab

      override def ensure[B](fab: Either[A, B])(error: => A)(predicate: B => Boolean): Either[A, B] =
        fab.ensure(error)(predicate)

      override def ensureOr[B](fab: Either[A, B])(error: B => A)(predicate: B => Boolean): Either[A, B] =
        fab.ensureOr(error)(predicate)

      override def reduceLeftToOption[B, C](fab: Either[A, B])(f: B => C)(g: (C, B) => C): Option[C] =
        fab.map(f).toOption

      override def reduceRightToOption[B, C](
        fab: Either[A, B]
      )(f: B => C)(g: (B, Eval[C]) => Eval[C]): Eval[Option[C]] =
        Now(fab.map(f).toOption)

      override def reduceLeftOption[B](fab: Either[A, B])(f: (B, B) => B): Option[B] =
        fab.toOption

      override def reduceRightOption[B](fab: Either[A, B])(f: (B, Eval[B]) => Eval[B]): Eval[Option[B]] =
        Now(fab.toOption)

      override def size[B](fab: Either[A, B]): Long =
        fab.fold(_ => 0L, _ => 1L)

      override def get[B](fab: Either[A, B])(idx: Long): Option[B] =
        if (idx == 0L) fab.fold(_ => None, Some(_)) else None

      override def foldMap[B, C](fab: Either[A, B])(f: B => C)(implicit C: Monoid[C]): C =
        fab.fold(_ => C.empty, f)

      override def find[B](fab: Either[A, B])(f: B => Boolean): Option[B] =
        fab.fold(_ => None, r => if (f(r)) Some(r) else None)

      override def exists[B](fab: Either[A, B])(p: B => Boolean): Boolean =
        fab.exists(p)

      override def forall[B](fab: Either[A, B])(p: B => Boolean): Boolean =
        fab.forall(p)

      override def toList[B](fab: Either[A, B]): List[B] =
        fab.fold(_ => Nil, _ :: Nil)

      override def isEmpty[B](fab: Either[A, B]): Boolean =
        fab.isLeft

      def functor: Functor[Either[A, *]] = this

      def align[B, C](fa: Either[A, B], fb: Either[A, C]): Either[A, Ior[B, C]] =
        alignWith(fa, fb)(identity)

      override def alignWith[B, C, D](fb: Either[A, B], fc: Either[A, C])(f: Ior[B, C] => D): Either[A, D] = fb match {
        case left @ Left(a) =>
          fc match {
            case Left(_)  => left.rightCast[D]
            case Right(c) => Right(f(Ior.right(c)))
          }
        case Right(b) =>
          fc match {
            case Left(a)  => Right(f(Ior.left(b)))
            case Right(c) => Right(f(Ior.both(b, c)))
          }
      }

    }
  // scalastyle:on method.length

  implicit def catsStdSemigroupKForEither[L]: SemigroupK[Either[L, *]] =
    new SemigroupK[Either[L, *]] {
      def combineK[A](x: Either[L, A], y: Either[L, A]): Either[L, A] = x match {
        case Left(_)  => y
        case Right(_) => x
      }
    }

  implicit def catsStdShowForEither[A, B](implicit A: Show[A], B: Show[B]): Show[Either[A, B]] =
    new Show[Either[A, B]] {
      def show(x: Either[A, B]): String =
        x match {
          case Left(a)  => "Left(" + A.show(a) + ")"
          case Right(b) => "Right(" + B.show(b) + ")"
        }
    }

  implicit def catsParallelForEitherAndValidated[E: Semigroup]: Parallel.Aux[Either[E, *], Validated[E, *]] =
    new Parallel[Either[E, *]] {
      type F[x] = Validated[E, x]

      def applicative: Applicative[Validated[E, *]] = Validated.catsDataApplicativeErrorForValidated
      def monad: Monad[Either[E, *]] = cats.instances.either.catsStdInstancesForEither

      def sequential: Validated[E, *] ~> Either[E, *] =
        λ[Validated[E, *] ~> Either[E, *]](_.toEither)

      def parallel: Either[E, *] ~> Validated[E, *] =
        λ[Either[E, *] ~> Validated[E, *]](_.toValidated)
    }
}
