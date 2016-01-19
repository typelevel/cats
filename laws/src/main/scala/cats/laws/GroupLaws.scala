package cats
package laws

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}

object GroupLaws {
  def apply[A : Eq : Arbitrary] = new GroupLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait GroupLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  // groups

  def semigroup(implicit A: Semigroup[A]) = new GroupProperties(
    name = "semigroup",
    parents = Nil,
    Rules.serializable(A),
    Rules.associativity(A.combine),
    Rules.repeat1("combineN")(A.combineN),
    Rules.repeat2("combineN", "|+|")(A.combineN)(A.combine)
  )

  def monoid(implicit A: Monoid[A]) = new GroupProperties(
    name = "monoid",
    parents = List(semigroup),
    Rules.leftIdentity(A.empty)(A.combine),
    Rules.rightIdentity(A.empty)(A.combine),
    Rules.repeat0("combineN", "id", A.empty)(A.combineN),
    Rules.collect0("combineAll", "id", A.empty)(A.combineAll),
    Rules.isId("isEmpty", A.empty)(A.isEmpty)
  )

  def group(implicit A: Group[A]) = new GroupProperties(
    name = "group",
    parents = List(monoid),
    Rules.leftInverse(A.empty)(A.combine)(A.inverse),
    Rules.rightInverse(A.empty)(A.combine)(A.inverse),
    Rules.consistentInverse("remove")(A.remove)(A.combine)(A.inverse)
  )

  // property classes

  class GroupProperties(
    val name: String,
    val parents: Seq[GroupProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Nil
  }

  class AdditiveProperties(
    val base: GroupProperties,
    val parents: Seq[AdditiveProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val name = base.name
    val bases = List("base" -> base)
  }

}
