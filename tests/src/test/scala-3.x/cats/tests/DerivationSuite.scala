package cats.tests

import cats.kernel.{Eq, Order, Monoid}

class DerivationSuite extends CatsSuite {
  enum Label derives Eq:
    case Green, Red, Black

  enum Tree derives Eq:
    case Leaf(label: Label)
    case Node(branches: List[Tree])

  test("Eq derivation (recursive and nested)") {
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
}
