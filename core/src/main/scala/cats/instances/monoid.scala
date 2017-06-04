package cats
package instances

trait MonoidInstances {

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
