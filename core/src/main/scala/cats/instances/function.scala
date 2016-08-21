package cats
package instances

import cats.arrow.{Arrow, Choice}
import cats.data.Xor
import cats.functor.Contravariant
import annotation.tailrec

private[instances] sealed trait Function0Instances {

  implicit val catsStdBimonadForFunction0: Bimonad[Function0] with RecursiveTailRecM[Function0] =
    new Bimonad[Function0] with RecursiveTailRecM[Function0] {
      def extract[A](x: () => A): A = x()

      def coflatMap[A, B](fa: () => A)(f: (() => A) => B): () => B =
        () => f(fa)

      def pure[A](x: A): () => A = () => x

      def flatMap[A, B](fa: () => A)(f: A => () => B): () => B =
        () => f(fa())()

      def tailRecM[A, B](a: A)(fn: A => () => Either[A, B]): () => B =
        () => {
          @tailrec
          def loop(thisA: A): B = fn(thisA)() match {
            case Right(b) => b
            case Left(nextA) => loop(nextA)
          }
          loop(a)
        }
    }

  implicit def catsStdEqForFunction0[A](implicit A: Eq[A]): Eq[() => A] =
    new Eq[() => A] {
      def eqv(x: () => A, y: () => A): Boolean = A.eqv(x(), y())
    }
}

private[instances] sealed trait Function1Instances extends Function1Instances0 {
  implicit def catsStdContravariantForFunction1[R]: Contravariant[? => R] =
    new Contravariant[? => R] {
      def contramap[T1, T0](fa: T1 => R)(f: T0 => T1): T0 => R =
        fa.compose(f)
    }

  implicit def catsStdMonadReaderForFunction1[T1]: MonadReader[T1 => ?, T1] with RecursiveTailRecM[T1 => ?] =
    new MonadReader[T1 => ?, T1] with RecursiveTailRecM[T1 => ?] {
      def pure[R](r: R): T1 => R = _ => r

      def flatMap[R1, R2](fa: T1 => R1)(f: R1 => T1 => R2): T1 => R2 =
        t => f(fa(t))(t)

      val ask: T1 => T1 = identity

      def local[A](f: T1 => T1)(fa: T1 => A): T1 => A = f.andThen(fa)

      override def map[R1, R2](fa: T1 => R1)(f: R1 => R2): T1 => R2 =
        f.compose(fa)

      def tailRecM[A, B](a: A)(fn: A => T1 => Either[A, B]): T1 => B =
        (t: T1) => {
          @tailrec
          def step(thisA: A): B = fn(thisA)(t) match {
            case Right(b) => b
            case Left(nextA) => step(nextA)
          }
          step(a)
        }
    }

  implicit val catsStdInstancesForFunction1: Choice[Function1] with Arrow[Function1] =
    new Choice[Function1] with Arrow[Function1] {
      def choice[A, B, C](f: A => C, g: B => C): Xor[A, B] => C =
        _ match {
          case Xor.Left(a)  => f(a)
          case Xor.Right(b) => g(b)
        }

      def lift[A, B](f: A => B): A => B = f

      def first[A, B, C](fa: A => B): ((A, C)) => (B, C) = {
        case (a, c) => (fa(a), c)
      }

      def id[A]: A => A = a => a

      override def split[A, B, C, D](f: A => B, g: C => D): ((A, C)) => (B, D) = {
        case (a, c) => (f(a), g(c))
      }

      def compose[A, B, C](f: B => C, g: A => B): A => C = f.compose(g)
    }

  implicit def catsStdMonoidForFunction1[A, B](implicit M: Monoid[B]): Monoid[A => B] =
    new Function1Monoid[A, B] { def B: Monoid[B] = M }

  implicit val catsStdMonoidKForFunction1: MonoidK[λ[α => α => α]] =
    new Function1MonoidK {}
}

private[instances] sealed trait Function1Instances0 {
  implicit def catsStdSemigroupForFunction1[A, B](implicit S: Semigroup[B]): Semigroup[A => B] =
    new Function1Semigroup[A, B] { def B: Semigroup[B] = S }

  implicit val catsStdSemigroupKForFunction1: SemigroupK[λ[α => α => α]] =
    new Function1SemigroupK {}
}

private[instances] sealed trait Function1Semigroup[A, B] extends Semigroup[A => B] {
  implicit def B: Semigroup[B]

  override def combine(x: A => B, y: A => B): A => B = { a =>
    B.combine(x(a), y(a))
  }
}

private[instances] sealed trait Function1Monoid[A, B] extends Monoid[A => B] with Function1Semigroup[A, B] {
  implicit def B: Monoid[B]

  override def empty: A => B = _ => B.empty
}

private[instances] sealed trait Function1SemigroupK extends SemigroupK[λ[α => α => α]] {
  override def combineK[A](x: A => A, y: A => A): A => A = x compose y
}

private[instances] sealed trait Function1MonoidK extends MonoidK[λ[α => α => α]] with Function1SemigroupK {
  override def empty[A]: A => A = identity[A]
}

trait FunctionInstances
  extends Function0Instances
  with Function1Instances
