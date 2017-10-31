package cats
package instances

trait EqInstances {
  implicit val catsDivisibleForEq: Divisible[Eq] =
    new Divisible[Eq] {
      /**
       * Defaults to the trivial equivalence relation
       * contracting the type to a point
       */
      def unit[A]: Eq[A] = Eq.allEqual

      /** Derive an `Eq` for `B` given an `Eq[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap2[A, B, C](fb: Eq[B], fc: Eq[C])(f: A => (B, C)): Eq[A] =
        Eq.instance { (l, r) =>
          (f(l), f(r)) match {
            case (derivedL, derivedR) =>
              fb.eqv(derivedL._1, derivedR._1) &&
                fc.eqv(derivedL._2, derivedR._2)
          }
        }
    }
}
