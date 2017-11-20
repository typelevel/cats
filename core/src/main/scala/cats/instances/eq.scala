package cats
package instances

trait EqInstances {
  implicit val catsContravariantSemigroupalForEq: ContravariantSemigroupal[Eq] =
    new ContravariantSemigroupal[Eq] {
      def contramap[A, B](fa: Eq[A])(fn: B => A): Eq[B] = Eq.by[B, A](fn)(fa)

      def product[A, B](fa: Eq[A], fb: Eq[B]): Eq[(A, B)] =
        Eq.instance { (left, right) => fa.eqv(left._1, right._1) && fb.eqv(left._2, right._2) }
    }
}
