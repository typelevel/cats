package cats.instances

import cats.kernel._
import cats.kernel.instances.unit._
import cats.{Invariant, InvariantMonoidal, InvariantSemigroupal, Monoid}

trait InvariantMonoidalInstances {

  implicit def catsSemigroupalForMonoid: InvariantSemigroupal[Monoid] = new InvariantSemigroupal[Monoid] {
    def product[A, B](fa: Monoid[A], fb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
      val empty = fa.empty -> fb.empty
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: Monoid[A])(f: A => B)(g: B => A): Monoid[B] = new Monoid[B] {
      def empty: B = f(fa.empty)

      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
    }
  }

  implicit val catsInvariantMonoidalSemigroup: InvariantMonoidal[Semigroup] = new InvariantMonoidal[Semigroup] {
    def product[A, B](fa: Semigroup[A], fb: Semigroup[B]): Semigroup[(A, B)] = new Semigroup[(A, B)] {
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: Semigroup[A])(f: A => B)(g: B => A): Semigroup[B] = new Semigroup[B] {
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
    }

    def unit: Semigroup[Unit] = implicitly
  }

  implicit val catsInvariantMonoidalCommutativeSemigroup: InvariantMonoidal[CommutativeSemigroup] =
    new InvariantMonoidal[CommutativeSemigroup] {
      def product[A, B](fa: CommutativeSemigroup[A], fb: CommutativeSemigroup[B]): CommutativeSemigroup[(A, B)] =
        new CommutativeSemigroup[(A, B)] {
          def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
        }

      def imap[A, B](fa: CommutativeSemigroup[A])(f: A => B)(g: B => A): CommutativeSemigroup[B] =
        new CommutativeSemigroup[B] {
          def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
        }

      def unit: CommutativeSemigroup[Unit] = implicitly
    }

  implicit val catsInvariantNumeric: Invariant[Numeric] = new Invariant[Numeric] {
    def imap[A, B](fa: Numeric[A])(f: A => B)(g: B => A): Numeric[B] = new Numeric[B] {
      def compare(x: B, y: B): Int = fa.compare(g(x), g(y))
      def plus(x: B, y: B): B = f(fa.plus(g(x), g(y)))
      def minus(x: B, y: B): B = f(fa.minus(g(x), g(y)))
      def times(x: B, y: B): B = f(fa.times(g(x), g(y)))
      def negate(x: B): B = f(fa.negate(g(x)))
      def fromInt(x: Int): B = f(fa.fromInt(x))
      def toInt(x: B): Int = fa.toInt(g(x))
      def toLong(x: B): Long = fa.toLong(g(x))
      def toFloat(x: B): Float = fa.toFloat(g(x))
      def toDouble(x: B): Double = fa.toDouble(g(x))
    }
  }
}
