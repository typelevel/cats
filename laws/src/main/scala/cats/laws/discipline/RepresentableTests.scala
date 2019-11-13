package cats.laws.discipline

import cats.{Eq, Representable}
import cats.laws.RepresentableLaws
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws
import org.scalacheck.Prop._

trait RepresentableTests[F[_], R] extends Laws {

  val laws: RepresentableLaws[F, R]

  def representable[A](implicit
                       ArbA: Arbitrary[A],
                       ArbFA: Arbitrary[F[A]],
                       ArbRep: Arbitrary[R],
                       ArbRepFn: Arbitrary[R => A],
                       EqFA: Eq[F[A]],
                       EqA: Eq[A]): RuleSet = new DefaultRuleSet(
    name = "representable",
    parent = None,
    "index andThen tabulate = id" -> forAll(laws.indexTabulateIsId[A] _),
    "tabulate andThen index = id" -> forAll(laws.tabulateIndexIsId[A] _)
  )
}

object RepresentableTests {
  def apply[F[_], R](implicit RF: Representable.Aux[F, R]): RepresentableTests[F, R] = new RepresentableTests[F, R] {
    implicit override val laws: RepresentableLaws[F, R] = RepresentableLaws[F, R]
  }
}
