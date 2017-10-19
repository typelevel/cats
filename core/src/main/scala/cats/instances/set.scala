package cats
package instances

import cats.kernel.CommutativeMonoid

import cats.syntax.show._

trait SetInstances extends cats.kernel.instances.SetInstances {

  implicit val catsStdInstancesForSet: UnorderedTraverse[Set] with MonoidK[Set] =
    new UnorderedTraverse[Set] with MonoidK[Set] {

      def unorderedTraverse[G[_]: CommutativeApplicative, A, B](sa: Set[A])(f: A => G[B]): G[Set[B]] =
        sa.foldLeft(Applicative[G].pure(Set.empty[B])) { (acc, a) =>
          Apply[G].map2(acc, f(a))(_ + _)
        }

      def sequenceUnordered[G[_]: CommutativeApplicative, A](sa: Set[G[A]]): G[Set[A]] =
        sa.foldLeft(Applicative[G].pure(Set.empty[A])) { (acc, a) =>
          Apply[G].map2(acc, a)(_ + _)
        }

      def empty[A]: Set[A] = Set.empty[A]

      def combineK[A](x: Set[A], y: Set[A]): Set[A] = x | y

      def foldLeft[A, B](fa: Set[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Set[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa.iterator, lb)(f)

      def insert[A](fa: Set[A], a: A): Set[A] = fa + a

      override def unorderedFold[A](fa: Set[A])(implicit A: CommutativeMonoid[A]): A = A.combineAll(fa)

      override def toSet[A](fa: Set[A]): Set[A] = fa

      override def forall[A](fa: Set[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: Set[A]): Boolean = fa.isEmpty

      override def find[A](fa: Set[A])(f: A => Boolean): Option[A] = fa.find(f)
    }

  implicit def catsStdShowForSet[A:Show]: Show[Set[A]] = new Show[Set[A]] {
    def show(fa: Set[A]): String =
      fa.toIterator.map(_.show).mkString("Set(", ", ", ")")
  }
}
