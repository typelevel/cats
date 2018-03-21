package cats
package instances

trait EquivInstances {
  implicit val catsContravariantMonoidalForEquiv: ContravariantMonoidal[Equiv] =
    new ContravariantMonoidal[Equiv] {
      /**
       * Defaults to trivially contracting the type
       * to a point
       */
      def unit: Equiv[Unit] = new Equiv[Unit] {
        def equiv(x: Unit, y: Unit): Boolean = true
      }

      /** Derive an `Equiv` for `B` given an `Equiv[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Equiv[A])(f: B => A): Equiv[B] =
        new Equiv[B] {
          def equiv(l: B, r: B): Boolean =
            fa.equiv(f(l), f(r))
        }

      def product[A, B](fa: Equiv[A], fb: Equiv[B]): Equiv[(A, B)] =
        new Equiv[(A, B)] {
          def equiv(l: (A, B), r: (A, B)): Boolean =
            fa.equiv(l._1, r._1) && fb.equiv(l._2, r._2)
        }
    }
}
