package cats
package instances

trait EquivInstances {
  implicit val catsDivisibleForEquiv: Divisible[Equiv] =
    new Divisible[Equiv] {
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
      def contramap2[A, B, C](fb: Equiv[B], fc: Equiv[C])(f: A => (B, C)): Equiv[A] =
        new Equiv[A] {
          def equiv(l: A, r: A): Boolean =
            (f(l), f(r)) match {
              case (derivedL, derivedR) =>
                fb.equiv(derivedL._1, derivedR._1) &&
                  fc.equiv(derivedL._2, derivedR._2)
            }
        }
    }
}
