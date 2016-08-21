package cats

import cats.functor.Invariant
import simulacrum.typeclass

/**
 * Invariant version of a Monoidal.
 *
 * Must obey the laws defined in cats.laws.InvariantMonoidalLaws.
 */
@typeclass trait InvariantMonoidal[F[_]] extends Invariant[F] with Cartesian[F] {
  def pure[A](a: A): F[A]
}

object InvariantMonoidal extends KernelInvariantMonoidalInstances

/**
 * InvariantMonoidal instances for types that are housed in cats.kernel and therefore
 * can't have instances for this type class in their companion objects.
 */
private[cats] trait KernelInvariantMonoidalInstances {
  implicit val catsInvariantMonoidalSemigroup: InvariantMonoidal[Semigroup] = new InvariantMonoidal[Semigroup] {
    def product[A, B](fa: Semigroup[A], fb: Semigroup[B]): Semigroup[(A, B)] = new Semigroup[(A, B)] {
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: Semigroup[A])(f: A => B)(g: B => A): Semigroup[B] = new Semigroup[B] {
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.map(g)).map(f)
    }

    def pure[A](a: A): Semigroup[A] = new Semigroup[A] {
      def combine(x: A, y: A): A = a
      override def combineAllOption(as: TraversableOnce[A]): Option[A] =
        if (as.isEmpty) None else Some(a)
    }
  }

  implicit val catsInvariantMonoidalMonoid: InvariantMonoidal[Monoid] = new InvariantMonoidal[Monoid] {
    def product[A, B](fa: Monoid[A], fb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
      val empty = fa.empty -> fb.empty
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: Monoid[A])(f: A => B)(g: B => A): Monoid[B] = new Monoid[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAll(bs: TraversableOnce[B]): B =
        f(fa.combineAll(bs.map(g)))
    }

    def pure[A](a: A): Monoid[A] = new Monoid[A] {
      val empty = a
      def combine(x: A, y: A): A = a
      override def combineAll(as: TraversableOnce[A]): A = a
    }
  }
}
