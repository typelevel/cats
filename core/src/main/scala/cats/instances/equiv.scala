package cats
package instances

trait EquivInstances {
  implicit val catsContravariantSemigroupalEquiv: ContravariantSemigroupal[Equiv] =
    new ContravariantSemigroupal[Equiv] {
      def contramap[A, B](fa: Equiv[A])(f: B => A): Equiv[B] =
        new Equiv[B] {
          def equiv(x: B, y: B): Boolean = fa.equiv(f(x), f(y))
        }

      def product[A, B](fa: Equiv[A], fb: Equiv[B]): Equiv[(A, B)] =
        new Equiv[(A, B)] {
          def equiv(x: (A, B), y: (A, B)): Boolean =
            fa.equiv(x._1, y._1) && fb.equiv(x._2, y._2)
        }
    }
}
