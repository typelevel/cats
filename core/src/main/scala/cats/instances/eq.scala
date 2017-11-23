package cats
package instances

trait EqInstances {
  implicit val catsContravariantMonoidalForEq: ContravariantMonoidal[Eq] =
    new ContravariantMonoidal[Eq] {
      /**
       * Defaults to the trivial equivalence relation
       * contracting the type to a point
       */
      def unit[A]: Eq[A] = Eq.allEqual

      /** Derive an `Eq` for `B` given an `Eq[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Eq[A])(f: B => A): Eq[B] =
        Eq.instance { (l: B, r: B) =>
          fa.eqv(f(l), f(r))
        }

      def product[A, B](fa: Eq[A], fb: Eq[B]): Eq[(A, B)] =
        Eq.instance { (l, r) =>
          (l, r) match {
            case ((aL, bL), (aR, bR)) =>
              fa.eqv(aL, aR) &&
                fb.eqv(bL, bR)
          }
        }
    }
}
