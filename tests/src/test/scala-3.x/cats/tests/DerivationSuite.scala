package cats.tests

import cats.kernel.{Eq, Order, Monoid, Semigroup}

class DerivationSuite extends CatsSuite {
  enum Label derives Eq:
    case Green, Red, Black

  enum Tree derives Eq:
    case Leaf(label: Label)
    case Node(branches: List[Tree])

  test("Eq derivation") {
    val derived = summon[Eq[Tree]]

    import Tree._, Label._

    assert(derived.eqv(Leaf(Red), Leaf(Red)))
    assert(!derived.eqv(Leaf(Red), Leaf(Black)))
    assert(!derived.eqv(Node(List(Leaf(Red), Leaf(Green))), Leaf(Black)))
    assert(derived.eqv(Node(List(Leaf(Red), Leaf(Green))),Node(List(Leaf(Red), Leaf(Green)))))

    assert(summon[Eq[Label]].eqv(Red, Red))
    assert(!summon[Eq[Label]].eqv(Green, Red))
    assert(!summon[Eq[Label]].eqv(Green, Black))
  }
  
  enum GTree[A] derives Eq:
    case Leaf(label: A)
    case Node(l: GTree[A], r: GTree[A])

  test("Eq derivation (generic)") {
    val derived = summon[Eq[GTree[Int]]]

    import GTree._

    assert(derived.eqv(Leaf(25), Leaf(25)))
    assert(!derived.eqv(Leaf(25), Node(Leaf(25), Leaf(0))))
    assert(derived.eqv(Node(Leaf(5), Leaf(1)), Node(Leaf(5), Leaf(1))))
  }

  case class Weight(kgs: Int) derives Monoid, Semigroup
  case class Price(amount: Double) derives Monoid, Semigroup
  case class Part(price: Price, weight: Weight) derives Monoid, Semigroup
  case object Empty derives Monoid, Semigroup

  test("Semigroup derivation") {
    val derived = summon[Semigroup[Weight]]

    assert(derived.combine(Weight(25), Weight(47)) == Weight(72))

    assert(summon[Semigroup[Part]].combine(
      Part(Price(25.0), Weight(11)), 
      Part(Price(3.0), Weight(25))) == Part(Price(28.0), Weight(36))
    )

    assert(summon[Semigroup[Empty.type]].combine(Empty, Empty) == Empty)
  }

  test("Monoid derivation") {
    val derived = summon[Monoid[Weight]]

    assert(derived.empty == Weight(0))

    assert(derived.combine(Weight(25), Weight(47)) == Weight(72))

    assert(summon[Monoid[Part]].combine(
      Part(Price(25.0), Weight(11)), 
      Part(Price(3.0), Weight(25))) == Part(Price(28.0), Weight(36))
    )

    assert(summon[Monoid[Empty.type]].combine(Empty, Empty) == Empty)
  }

  enum Difficulty derives Order:
    case Low, Medium, Hard

  
  case class Problem(naming: Difficulty, implementing: Difficulty) derives Order

  test("Order derivation") { 
    {
      val ord = summon[Order[Difficulty]]

      import Difficulty._

      assert(ord.compare(Low, Hard) == -1)
      assert(ord.compare(Low, Low) == 0)
      assert(ord.compare(Medium, Low) == 1)
    }
    {
      val ord = summon[Order[Problem]]
      import Difficulty._

      assert(ord.compare(Problem(Low, Low), Problem(Low, Low)) == 0)
      assert(ord.compare(Problem(Hard, Low), Problem(Medium, Low)) > 0)
      assert(ord.compare(Problem(Hard, Medium), Problem(Hard, Hard)) < 0)
    }
  }
}
