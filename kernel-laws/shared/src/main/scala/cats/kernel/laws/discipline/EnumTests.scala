package cats
package kernel
package laws
package discipline

import cats.kernel.instances.boolean._
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll

trait PartialNextTests[A] extends PartialOrderTests[A] {

  def laws: PartialNextLaws[A]

  def partialNext(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "partialNext",
      Some(partialOrder),
      "next(a) > a" -> forAll(laws.nextOrderWeak _),
      "forall a, b. if a < b. next(a) <= b" -> forAll(laws.nextOrderStrong _)
    )

}

trait PartialPreviousTests[A] extends PartialOrderTests[A] {

  def laws: PartialPreviousLaws[A]

  def partialPrevious(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "partialPrevious",
      Some(partialOrder),
      "next(a) > a" -> forAll(laws.previousOrderWeak _),
      "forall a, b. if a < b. next(a) <= b" -> forAll(laws.previousOrderStrong _)
    )

}

trait BoundedEnumTests[A] extends OrderTests[A] with PartialNextTests[A] with PartialPreviousTests[A] {

  def laws: BoundedEnumLaws[A]

  def boundedEnum(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqOA: Eq[Option[A]], eqA: Eq[A]): RuleSet =
    new RuleSet {
      val name: String = "boundedEnum"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(partialNext, partialPrevious, order)
      val props: Seq[(String, Prop)] = Seq(
        "min bound is terminal" -> laws.minBoundTerminal,
        "max bound is terminal" -> laws.maxBoundTerminal,
        "partial right identity" -> forAll(laws.partialRightIdentity _),
        "partial left identity" -> forAll(laws.partialLeftIdentity _)
      )
    }

}

object BoundedEnumTests {
  def apply[A: BoundedEnum]: BoundedEnumTests[A] =
    new BoundedEnumTests[A] { def laws: BoundedEnumLaws[A] = BoundedEnumLaws[A] }
}
