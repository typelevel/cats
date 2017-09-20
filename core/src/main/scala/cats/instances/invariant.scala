package cats.instances

import cats.kernel._
import cats.InvariantMonoidal

trait InvariantMonoidalInstances {

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
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.map(g)).map(f)
    }

    def pure[A](a: A): Monoid[A] = new Monoid[A] {
      val empty = a
      def combine(x: A, y: A): A = a
      override def combineAllOption(as: TraversableOnce[A]): Option[A] =
        if (as.isEmpty) None else Some(a)
    }
  }

  implicit val catsInvariantMonoidalBand: InvariantMonoidal[Band] = new InvariantMonoidal[Band] {
    def product[A, B](fa: Band[A], fb: Band[B]): Band[(A, B)] = new Band[(A, B)] {
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: Band[A])(f: A => B)(g: B => A): Band[B] = new Band[B] {
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.map(g)).map(f)
    }

    def pure[A](a: A): Band[A] = new Band[A] {
      def combine(x: A, y: A): A = a
      override def combineAllOption(as: TraversableOnce[A]): Option[A] =
        if (as.isEmpty) None else Some(a)
    }
  }

  implicit val catsInvariantMonoidalSemilattice: InvariantMonoidal[Semilattice] = new InvariantMonoidal[Semilattice] {
    def product[A, B](fa: Semilattice[A], fb: Semilattice[B]): Semilattice[(A, B)] = new Semilattice[(A, B)] {
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: Semilattice[A])(f: A => B)(g: B => A): Semilattice[B] = new Semilattice[B] {
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.map(g)).map(f)
    }

    def pure[A](a: A): Semilattice[A] = new Semilattice[A] {
      def combine(x: A, y: A): A = a
      override def combineAllOption(as: TraversableOnce[A]): Option[A] =
        if (as.isEmpty) None else Some(a)
    }
  }

  implicit val catsInvariantMonoidalCommutativeSemigroup: InvariantMonoidal[CommutativeSemigroup] = new InvariantMonoidal[CommutativeSemigroup] {
    def product[A, B](fa: CommutativeSemigroup[A], fb: CommutativeSemigroup[B]): CommutativeSemigroup[(A, B)] = new CommutativeSemigroup[(A, B)] {
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: CommutativeSemigroup[A])(f: A => B)(g: B => A): CommutativeSemigroup[B] = new CommutativeSemigroup[B] {
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.map(g)).map(f)
    }

    def pure[A](a: A): CommutativeSemigroup[A] = new CommutativeSemigroup[A] {
      def combine(x: A, y: A): A = a
      override def combineAllOption(as: TraversableOnce[A]): Option[A] =
        if (as.isEmpty) None else Some(a)
    }
  }

  implicit val catsInvariantMonoidalCommutativeMonoid: InvariantMonoidal[CommutativeMonoid] = new InvariantMonoidal[CommutativeMonoid] {
    def product[A, B](fa: CommutativeMonoid[A], fb: CommutativeMonoid[B]): CommutativeMonoid[(A, B)] = new CommutativeMonoid[(A, B)] {
      val empty = fa.empty -> fb.empty
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: CommutativeMonoid[A])(f: A => B)(g: B => A): CommutativeMonoid[B] = new CommutativeMonoid[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.map(g)).map(f)
    }

    def pure[A](a: A): CommutativeMonoid[A] = new CommutativeMonoid[A] {
      val empty = a
      def combine(x: A, y: A): A = a
      override def combineAll(as: TraversableOnce[A]): A = a
    }
  }

  implicit val catsInvariantMonoidalBoundedSemilattice: InvariantMonoidal[BoundedSemilattice] = new InvariantMonoidal[BoundedSemilattice] {
    def product[A, B](fa: BoundedSemilattice[A], fb: BoundedSemilattice[B]): BoundedSemilattice[(A, B)] = new BoundedSemilattice[(A, B)] {
      val empty = fa.empty -> fb.empty
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: BoundedSemilattice[A])(f: A => B)(g: B => A): BoundedSemilattice[B] = new BoundedSemilattice[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.map(g)).map(f)
    }

    def pure[A](a: A): BoundedSemilattice[A] = new BoundedSemilattice[A] {
      val empty = a
      def combine(x: A, y: A): A = a
      override def combineAllOption(as: TraversableOnce[A]): Option[A] =
        if (as.isEmpty) None else Some(a)
    }
  }


  implicit val catsInvariantMonoidalGroup: InvariantMonoidal[Group] = new InvariantMonoidal[Group] {
    def product[A, B](fa: Group[A], fb: Group[B]): Group[(A, B)] = new Group[(A, B)] {
      def inverse(ab: (A, B)) = ab match { case (a, b) => (fa.inverse(a), fb.inverse(b)) }
      val empty = fa.empty -> fb.empty
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: Group[A])(f: A => B)(g: B => A): Group[B] = new Group[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      def inverse(b: B): B = f(fa.inverse(g(b)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.map(g)).map(f)
    }

    def pure[A](a: A): Group[A] = new Group[A] {
      val empty = a
      def combine(x: A, y: A): A = a
      override def combineAllOption(as: TraversableOnce[A]): Option[A] =
        if (as.isEmpty) None else Some(a)
      def inverse(a: A): A = a
    }
  }

  implicit val catsInvariantMonoidalCommutativeGroup: InvariantMonoidal[CommutativeGroup] = new InvariantMonoidal[CommutativeGroup] {
    def product[A, B](fa: CommutativeGroup[A], fb: CommutativeGroup[B]): CommutativeGroup[(A, B)] = new CommutativeGroup[(A, B)] {
      def inverse(ab: (A, B)) = ab match { case (a, b) => (fa.inverse(a), fb.inverse(b)) }
      val empty = fa.empty -> fb.empty
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: CommutativeGroup[A])(f: A => B)(g: B => A): CommutativeGroup[B] = new CommutativeGroup[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      def inverse(b: B): B = f(fa.inverse(g(b)))
      override def combineAllOption(bs: TraversableOnce[B]): Option[B] =
        fa.combineAllOption(bs.map(g)).map(f)
    }

    def pure[A](a: A): CommutativeGroup[A] = new CommutativeGroup[A] {
      val empty = a
      def combine(x: A, y: A): A = a
      override def combineAllOption(as: TraversableOnce[A]): Option[A] =
        if (as.isEmpty) None else Some(a)
      def inverse(a: A): A = a
    }
  }


}
