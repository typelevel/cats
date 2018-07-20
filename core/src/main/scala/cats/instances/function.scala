package cats
package instances

import cats.Contravariant
import cats.arrow.{Category, ArrowChoice, CommutativeArrow}

import annotation.tailrec


trait FunctionInstances extends cats.kernel.instances.FunctionInstances
    with Function0Instances with Function1Instances

trait FunctionInstancesBinCompat0 {
  /**
   * Witness for: E => A <-> E => A
   */
  implicit def catsStdRepresentableForFunction1[E](implicit EF: Functor[E => ?]): Representable.Aux[E => ?, E] = new Representable[E => ?] {
    override type Representation = E
    override val F: Functor[E => ?] = EF
    override def tabulate[A](f: E => A): E => A = f
    override def index[A](f: E => A): E => A = f
  }

  implicit val catsSddDeferForFunction0: Defer[Function0] =
    new Defer[Function0] {
      case class Deferred[A](fa: () => Function0[A]) extends Function0[A] {
        def apply() = {
          @annotation.tailrec
          def loop(f: () => Function0[A]): A =
            f() match {
              case Deferred(f) => loop(f)
              case next => next()
            }
          loop(fa)
        }
      }
      def defer[A](fa: => Function0[A]): Function0[A] = {
        lazy val cachedFa = fa
        Deferred(() => cachedFa)
      }
    }

  implicit def catsStdDeferForFunction1[A]: Defer[A => ?] =
    new Defer[A => ?] {
      case class Deferred[B](fa: () => A => B) extends (A => B) {
        def apply(a: A) = {
          @annotation.tailrec
          def loop(f: () => A => B): B =
            f() match {
              case Deferred(f) => loop(f)
              case next => next(a)
            }
          loop(fa)
        }
      }
      def defer[B](fa: => A => B): A => B = {
        lazy val cachedFa = fa
        Deferred(() => cachedFa)
      }
    }
}

private[instances] sealed trait Function0Instances extends Function0Instances0 {
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

private[instances] sealed trait Function0Instances0 {
   implicit def function0Distributive: Distributive[Function0] = new Distributive[Function0] {
    def distribute[F[_]: Functor, A, B](fa: F[A])(f: A => Function0[B]): Function0[F[B]] = {() => Functor[F].map(fa)(a => f(a)()) }

    def map[A, B](fa: Function0[A])(f: A => B): Function0[B] = () => f(fa())
  }
}

private[instances] sealed trait Function1Instances extends Function1Instances0 {
  implicit def catsStdContravariantMonoidalForFunction1[R: Monoid]: ContravariantMonoidal[? => R] =
    new ContravariantMonoidal[? => R] {
      def unit: Unit => R = Function.const(Monoid[R].empty)
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

  implicit val catsStdInstancesForFunction1: ArrowChoice[Function1] with CommutativeArrow[Function1] =
    new ArrowChoice[Function1] with CommutativeArrow[Function1] {
      def choose[A, B, C, D](f: A => C)(g: B => D): Either[A, B] => Either[C, D] =
        _ match {
          case Left(a) => Left(f(a))
          case Right(b) => Right(g(b))
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

  implicit def catsStdDistributiveForFunction1[T1]: Distributive[T1 => ?] = new Distributive[T1 => ?] {
    def distribute[F[_]: Functor, A, B](fa: F[A])(f: A => (T1 => B)): T1 => F[B] = {t1 => Functor[F].map(fa)(a => f(a)(t1)) }

    def map[A, B](fa: T1 => A)(f: A => B): T1 => B = {t1 => f(fa(t1))}
  }
}
