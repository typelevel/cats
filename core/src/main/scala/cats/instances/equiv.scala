package cats
package instances

trait EquivInstances {
  implicit val catsContravariantMonoidalForEquiv: ContravariantMonoidal[Equiv] =
    new ContravariantMonoidal[Equiv] {
      /**
       * Defaults to trivially contracting the type
       * to a point
       */
      def unit[A]: Equiv[A] = new Equiv[A] {
        def equiv(x: A, y: A): Boolean = true
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
            (l, r) match {
              case ((aL, bL), (aR, bR)) =>
                fa.equiv(aL, aR) &&
                  fb.equiv(bR, bL)
            }
        }
    }
}
