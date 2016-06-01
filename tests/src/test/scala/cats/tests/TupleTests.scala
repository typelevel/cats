package cats
package tests

import cats.laws.discipline.{BitraverseTests, SerializableTests}

class TupleTests extends CatsSuite {
  checkAll("Tuple2", BitraverseTests[Tuple2].bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[Tuple2]", SerializableTests.serializable(Bitraverse[Tuple2]))

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
