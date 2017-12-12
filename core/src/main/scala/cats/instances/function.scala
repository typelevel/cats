package cats
package instances

import cats.Contravariant
import cats.arrow.{Category, Choice, CommutativeArrow}

import annotation.tailrec


trait FunctionInstances extends cats.kernel.instances.FunctionInstances
    with Function0Instances with Function1Instances

private[instances] sealed trait Function0Instances {
  implicit val catsStdBimonadForFunction0: Bimonad[Function0] =
    new Bimonad[Function0] {
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
}

private[instances] sealed trait Function1Instances extends Function1Instances0 {
  implicit def catsStdContravariantMonoidalForFunction1[R: Monoid]: ContravariantMonoidal[? => R] =
    new ContravariantMonoidal[? => R] {
      def unit[A]: A => R = Function.const(Monoid[R].empty)
      def contramap[A, B](fa: A => R)(f: B => A): B => R =
        fa compose f
      def product[A, B](fa: A => R, fb: B => R): ((A, B)) => R =
        (ab: (A, B)) => ab match {
          case (a, b) => Monoid[R].combine(fa(a), fb(b))
        }
    }

  implicit def catsStdMonadForFunction1[T1]: Monad[T1 => ?] =
    new Monad[T1 => ?] {
      def pure[R](r: R): T1 => R = _ => r

      def flatMap[R1, R2](fa: T1 => R1)(f: R1 => T1 => R2): T1 => R2 =
        t => f(fa(t))(t)

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

  implicit val catsStdInstancesForFunction1: Choice[Function1] with CommutativeArrow[Function1] =
    new Choice[Function1] with CommutativeArrow[Function1] {
      def choice[A, B, C](f: A => C, g: B => C): Either[A, B] => C = {
        case Left(a)  => f(a)
        case Right(b) => g(b)
      }

      def lift[A, B](f: A => B): A => B = f

      def first[A, B, C](fa: A => B): ((A, C)) => (B, C) = {
        case (a, c) => (fa(a), c)
      }

      override def split[A, B, C, D](f: A => B, g: C => D): ((A, C)) => (B, D) = {
        case (a, c) => (f(a), g(c))
      }

      def compose[A, B, C](f: B => C, g: A => B): A => C = f.compose(g)
    }

  implicit val catsStdMonoidKForFunction1: MonoidK[Endo] =
    Category[Function1].algebraK
}


private[instances] sealed trait Function1Instances0 {
  implicit def catsStdContravariantForFunction1[R]: Contravariant[? => R] =
    new Contravariant[? => R] {
      def contramap[T1, T0](fa: T1 => R)(f: T0 => T1): T0 => R =
        fa.compose(f)
    }
}
