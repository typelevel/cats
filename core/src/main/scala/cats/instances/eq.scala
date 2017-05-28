package cats
package instances

trait EqInstances {
  implicit val catsContravariantCartesianEq: ContravariantCartesian[Eq] = new ContravariantCartesian[Eq] {
    def contramap[A, B](fa: Eq[A])(fn: B => A): Eq[B] = fa.on(fn)
    def product[A, B](fa: Eq[A], fb: Eq[B]): Eq[(A, B)] =
      Eq.instance { (left, right) => fa.eqv(left._1, right._1) && fb.eqv(left._2, right._2) }
  }
}
