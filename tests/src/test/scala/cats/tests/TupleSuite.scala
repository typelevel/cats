package cats
package tests

import data.NonEmptyList

import cats.laws.discipline._
import cats.laws.discipline.arbitrary._

class TupleSuite extends CatsSuite {

  implicit val iso1 = SemigroupalTests.Isomorphisms.invariant[(NonEmptyList[Int], ?)]
  implicit val iso2 = SemigroupalTests.Isomorphisms.invariant[(String, ?)]

  checkAll("Tuple2", BitraverseTests[Tuple2].bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[Tuple2]", SerializableTests.serializable(Bitraverse[Tuple2]))

  checkAll("Tuple2[String, Int] with Option", TraverseTests[(String, ?)].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[(String, ?)]", SerializableTests.serializable(Traverse[(String, ?)]))

  checkAll("Tuple2[String, Int]", ComonadTests[(String, ?)].comonad[Int, Int, Int])
  checkAll("Comonad[(String, ?)]", SerializableTests.serializable(Comonad[(String, ?)]))

  // Note that NonEmptyList has no Monoid, so we can make a FlatMap, but not a Monad
  checkAll("FlatMap[(NonEmptyList[Int], ?)]", FlatMapTests[(NonEmptyList[Int], ?)].flatMap[String, Long, String])
  checkAll("FlatMap[(String, ?)] serializable", SerializableTests.serializable(FlatMap[(String, ?)]))

  checkAll("Monad[(String, ?)]", MonadTests[(String, ?)].monad[Int, Int, String])
  checkAll("Monad[(String, ?)] serializable", SerializableTests.serializable(Monad[(String, ?)]))

  checkAll("Tuple2[String, Int]", ReducibleTests[(String, ?)].reducible[Option, Int, Int])
  checkAll("Reducible[(String, ?)]", SerializableTests.serializable(Reducible[(String, ?)]))

  test("Semigroupal composition") {
    val cart = ContravariantSemigroupal[Eq].composeFunctor[(Int, ?)]
    val eq = cart.product(Eq[(Int, String)], Eq[(Int, Int)])
    forAll { (a: (Int, (String, Int)), b: (Int, (String, Int))) =>
      (a == b) should === (eq.eqv(a, b))
    }
  }

  test("eqv") {
    val eq = Eq[(Int, Long)]
    forAll { t: (Int, Long) => eq.eqv(t, t) should === (true) }
    forAll { t: (Int, Long) => eq.eqv(t, t._1 -> (t._2 + 1)) should === (false) }
  }

  test("order") {
    forAll { t: (Int, Int) =>
      val u = t.swap
      Order[(Int, Int)].compare(t, u) should === (scala.math.Ordering[(Int, Int)].compare(t, u))
    }
  }

  test("show") {
    (1, 2).show should === ("(1,2)")

    forAll { fs: (String, String) =>
      fs.show should === (fs.toString)
    }

    // Provide some "non-standard" Show instances to make sure the tuple2 is actually use the Show instances for the
    // relevant types instead of blindly calling toString
    case class Foo(x: Int)
    implicit val fooShow: Show[Foo] = new Show[Foo] {
      override def show(f: Foo): String = s"foo.x = ${f.x}"
    }
    case class Bar(y: Int)
    implicit val barShow: Show[Bar] = new Show[Bar] {
      override def show(f: Bar): String = s"bar.y = ${f.y}"
    }

    val foo = Foo(1)
    val bar = Bar(2)
    (foo, bar).show should === (s"(${fooShow.show(foo)},${barShow.show(bar)})")
  }
}
