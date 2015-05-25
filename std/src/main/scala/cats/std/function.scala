package cats
package std

import algebra.Eq
import cats.arrow.Arrow
import cats.data.Xor
import cats.functor.{ProChoice, Contravariant}

trait Function0Instances {
  implicit val function0Instance: Bimonad[Function0] =
    new Bimonad[Function0] {
      def extract[A](x: () => A): A = x()

      def coflatMap[A, B](fa: () => A)(f: (() => A) => B): () => B =
        () => f(fa)

      def pure[A](x: A): () => A = () => x

      def flatMap[A, B](fa: () => A)(f: A => () => B): () => B =
        () => f(fa())()
    }

  implicit def eqFunction0[A](implicit A: Eq[A]): Eq[() => A] =
    new Eq[() => A] {
      def eqv(x: () => A, y: () => A): Boolean = A.eqv(x(), y())
    }
}

trait Function1Instances extends Function1Instances0 {
  implicit def function1Contravariant[R]: Contravariant[? => R] =
    new Contravariant[? => R] {
      def contramap[T1, T0](fa: T1 => R)(f: T0 => T1): T0 => R =
        fa.compose(f)
    }

  implicit def function1Covariant[T1]: Monad[T1 => ?] =
    new Monad[T1 => ?] {
      def pure[R](r: R): T1 => R = _ => r

      def flatMap[R1, R2](fa: T1 => R1)(f: R1 => T1 => R2): T1 => R2 =
        t => f(fa(t))(t)

      override def map[R1, R2](fa: T1 => R1)(f: R1 => R2): T1 => R2 =
        f.compose(fa)
    }

  implicit val function1Arrow: Arrow[Function1] =
    new Arrow[Function1] {
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

  implicit def function1Monoid[A,B](implicit B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def empty: A => B = _ => B.empty
      def combine(x: A => B, y: A => B): A => B = { a =>
        B.combine(x(a), y(a))
      }
    }
}

sealed trait Function1Instances0 {
  implicit val function1ProChoice: ProChoice[Function1] = new ProChoice[Function1] {
    override def left[A, B, C](fab: A => B): Xor[A, C] => Xor[B, C] =
      _.leftMap(fab)
    override def right[A, B, C](fab: A => B): Xor[C, A] => Xor[C, B] =
      _.map(fab)
    override def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D =
      g compose fab compose f
  }
}

trait FunctionInstances
  extends Function0Instances
  with Function1Instances
