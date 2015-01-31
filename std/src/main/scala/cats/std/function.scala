package cats
package std

import cats.arrow.Arrow
import cats.functor.Contravariant

trait Function1Instances {
  implicit def function1Contravariant[R]: Contravariant[? => R] =
    new Contravariant[? => R] {
      def contramap[T1, T0](fa: T1 => R)(f: T0 => T1): T0 => R =
        fa.compose(f)
    }

  implicit def function1Covariant[T1]: Functor[T1 => ?] =
    new Functor[T1 => ?] {
      def map[R1, R2](fa: T1 => R1)(f: R1 => R2): T1 => R2 =
        f.compose(fa)
    }

  implicit val function1Instance: Arrow[Function1] =
    new Arrow[Function1] {
      def lift[A, B](f: A => B): A => B = f

      def first[A, B, C](fa: A => B): ((A, C)) => (B, C) = {
        case (a, c) => (fa(a), c)
      }

      def id[A]: A => A = a => a

      def split[A, B, C, D](f: A => B, g: C => D): ((A, C)) => (B, D) = {
        case (a, c) => (f(a), g(c))
      }

      def compose[A, B, C](f: B => C, g: A => B): A => C = f.compose(g)
    }
}
