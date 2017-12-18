package cats
package instances

trait PartialOrderingInstances {
  implicit val catsContravariantSemigroupalForPartialOrdering: ContravariantSemigroupal[PartialOrdering] =
    new ContravariantSemigroupal[PartialOrdering] {
      /** Derive a `PartialOrdering` for `B` given a `PartialOrdering[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: PartialOrdering[A])(f: B => A): PartialOrdering[B] =
        new PartialOrdering[B] {
          def lteq(x: B, y: B): Boolean = fa.lteq(f(x), f(y))
          def tryCompare(x: B, y: B): Option[Int] = fa.tryCompare(f(x), f(y))
        }

      def product[A, B](fa: PartialOrdering[A], fb: PartialOrdering[B]): PartialOrdering[(A, B)] =
        new PartialOrdering[(A, B)] {
          def lteq(x: (A, B), y: (A, B)): Boolean =
            tryCompare(x, y).exists(_ <= 0)
          def tryCompare(x: (A, B), y: (A, B)): Option[Int] =
            fa.tryCompare(x._1, y._1) match {
              case Some(0) => fb.tryCompare(x._2, y._2)
              case option => option
            }
        }
    }
}
