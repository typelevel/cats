package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._
import org.typelevel.discipline.Laws

trait CoflatMapTests[F[_]] extends Laws with FunctorTests[F] {
  def laws: CoflatMapLaws[F]

  def coflatMap[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
                                                          ArbFA: Arbitrary[F[A]],
                                                          CogenA: Cogen[A],
                                                          CogenB: Cogen[B],
                                                          CogenC: Cogen[C],
                                                          CogenFA: Cogen[F[A]],
                                                          CogenFB: Cogen[F[B]],
                                                          EqFA: Eq[F[A]],
                                                          EqFC: Eq[F[C]],
                                                          EqFFA: Eq[F[F[A]]],
                                                          EqFB: Eq[F[B]],
                                                          EqFFFA: Eq[F[F[F[A]]]]): RuleSet =
    new DefaultRuleSet(
      name = "coflatMap",
      parent = Some(functor[A, B, C]),
      "coflatMap associativity" -> forAll(laws.coflatMapAssociativity[A, B, C] _),
      "coflatMap identity" -> forAll(laws.coflatMapIdentity[A, B] _),
      "coflatten coherence" -> forAll(laws.coflattenCoherence[A, B] _),
      "coflatten throughMap" -> forAll(laws.coflattenThroughMap[A] _)
    )
}

object CoflatMapTests {
  def apply[F[_]: CoflatMap]: CoflatMapTests[F] =
    new CoflatMapTests[F] { def laws: CoflatMapLaws[F] = CoflatMapLaws[F] }
}
