package cats
package instances

trait SemigroupInstances {

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

}
