package cats
package instances

import cats.functor.Contravariant

trait PartialOrderingInstances {
  implicit val catsFunctorContravariantForPartialOrdering: Contravariant[PartialOrdering] =
    new Contravariant[PartialOrdering] {
      /** Derive a `PartialOrdering` for `B` given a `PartialOrdering[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: PartialOrdering[A])(f: B => A): PartialOrdering[B] =
        new PartialOrdering[B] {
          def lteq(x: B, y: B): Boolean = fa.lteq(f(x), f(y))
          def tryCompare(x: B, y: B): Option[Int] = fa.tryCompare(f(x), f(y))
        }
    }
}
