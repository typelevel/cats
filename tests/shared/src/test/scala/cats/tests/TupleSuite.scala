/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats.{
  Bitraverse,
  CommutativeFlatMap,
  CommutativeMonad,
  Comonad,
  ContravariantSemigroupal,
  FlatMap,
  Invariant,
  Monad,
  Reducible,
  Show,
  Traverse
}
import cats.data.NonEmptyList
import cats.kernel.{Eq, Order}
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.show.*
import cats.tests.Helpers.{CMono, CSemi, Mono, Semi}
import cats.syntax.eq.*
import org.scalacheck.Prop.*

class TupleSuite extends CatsSuite {

  implicit val iso1: Isomorphisms[(NonEmptyList[Int], *)] = Isomorphisms.invariant[(NonEmptyList[Int], *)]
  implicit val iso2: Isomorphisms[(String, *)] = Isomorphisms.invariant[(String, *)]

  checkAll("Tuple2", BitraverseTests[Tuple2].bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[Tuple2]", SerializableTests.serializable(Bitraverse[Tuple2]))
  checkAll("Tuple4",
           BitraverseTests[(Boolean, Boolean, *, *)].bitraverse[Option, Int, Int, Int, String, String, String]
  )
  checkAll("Bitraverse[Tuple4]", SerializableTests.serializable(Bitraverse[(Boolean, Boolean, *, *)]))

  checkAll("Traverse[Tuple1] with Option", TraverseTests[Tuple1].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Tuple1]", SerializableTests.serializable(Traverse[Tuple1]))
  checkAll("Traverse[String, Int] with Option", TraverseTests[(String, *)].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[(String, *)]", SerializableTests.serializable(Traverse[(String, *)]))
  checkAll(
    "Traverse[Boolean, Boolean, String, Double, List[Int], Int] with Option",
    TraverseTests[(Boolean, Boolean, String, Double, List[Int], *)].traverse[Int, Int, Int, Int, Option, Option]
  )
  checkAll("Traverse[(Boolean, Boolean, String, Double, List[Int], *)]",
           SerializableTests.serializable(Traverse[(Boolean, Boolean, String, Double, List[Int], *)])
  )

  checkAll("Comonad[Tuple1]", ComonadTests[Tuple1].comonad[Int, Int, Int])
  checkAll("Comonad[Tuple1]", SerializableTests.serializable(Comonad[Tuple1]))
  checkAll("Comonad[(String, *)]", ComonadTests[(String, *)].comonad[Int, Int, Int])
  checkAll("Comonad[(String, *)]", SerializableTests.serializable(Comonad[(String, *)]))
  checkAll("Comonad[(Boolean, Boolean, String, String, List[Boolean], *)]",
           ComonadTests[(Boolean, Boolean, String, String, List[Boolean], *)].comonad[Int, Int, Int]
  )
  checkAll(
    "Comonad[(Boolean, Boolean, String, String, List[Boolean], *)]",
    SerializableTests.serializable(Comonad[(Boolean, Boolean, String, String, List[Boolean], *)])
  )

  checkAll("Invariant[Tuple1]", InvariantTests[Tuple1].invariant[String, Long, String])
  checkAll("Invariant[Tuple1] serializable", SerializableTests.serializable(Invariant[Tuple1]))
  checkAll("Invariant[(String, *)]", InvariantTests[(String, *)].invariant[String, Long, String])
  checkAll("Invariant[(String, *)] serializable", SerializableTests.serializable(Invariant[(String, *)]))
  checkAll("Invariant[(Int, String, Double, *)]",
           InvariantTests[(Int, String, Double, List[Int], Boolean, *)].invariant[String, Long, String]
  )
  checkAll(
    "Invariant[(Int, String, Double, List[Int], Boolean, *)] serializable",
    SerializableTests.serializable(Invariant[(Int, String, Double, List[Int], Boolean, *)])
  )

  checkAll("FlatMap[Tuple1]", FlatMapTests[Tuple1].flatMap[String, Long, String])
  checkAll("FlatMap[Tuple1] serializable", SerializableTests.serializable(FlatMap[Tuple1]))
  // Note that NonEmptyList has no Monoid, so we can make a FlatMap, but not a Monad
  checkAll("FlatMap[(NonEmptyList[Int], *)]", FlatMapTests[(NonEmptyList[Int], *)].flatMap[String, Long, String])
  checkAll("FlatMap[(String, *)] serializable", SerializableTests.serializable(FlatMap[(String, *)]))
  checkAll("FlatMap[(Semi, Semi, NonEmptyList[Int], *)]",
           FlatMapTests[(Semi, Semi, NonEmptyList[Int], Semi, NonEmptyList[Int], *)].flatMap[String, Long, String]
  )
  checkAll("FlatMap[(Semi, Semi, String, *)] serializable",
           SerializableTests.serializable(FlatMap[(Semi, Semi, String, Semi, NonEmptyList[Int], *)])
  )

  checkAll("Monad[Tuple1]", MonadTests[Tuple1].monad[Int, Int, String])
  checkAll("Monad[Tuple1] serializable", SerializableTests.serializable(Monad[Tuple1]))
  checkAll("Monad[(String, *)]", MonadTests[(String, *)].monad[Int, Int, String])
  checkAll("Monad[(String, *)] serializable", SerializableTests.serializable(Monad[(String, *)]))
  checkAll("Monad[(Mono, Mono, String, Mono, String, *)]",
           MonadTests[(Mono, Mono, String, Mono, String, *)].monad[Int, Int, String]
  )
  checkAll("Monad[(Mono, Mono, String, Mono, String, *)] serializable",
           SerializableTests.serializable(Monad[(Mono, Mono, String, Mono, String, *)])
  )

  checkAll("CommutativeFlatMap[Tuple1]", CommutativeFlatMapTests[Tuple1].commutativeFlatMap[String, Int, String])
  checkAll("CommutativeFlatMap[Tuple1] serializable", SerializableTests.serializable(CommutativeFlatMap[Tuple1]))
  checkAll("CommutativeFlatMap[(CSemi, *)]", CommutativeFlatMapTests[(CSemi, *)].commutativeFlatMap[Int, Int, String])
  checkAll("CommutativeFlatMap[(CSemi, *)] serializable",
           SerializableTests.serializable(CommutativeFlatMap[(CSemi, *)])
  )
  checkAll(
    "CommutativeFlatMap[(CSemi, CSemi, CSemi, CSemi, CSemi, *)]",
    CommutativeFlatMapTests[(CSemi, CSemi, CSemi, CSemi, CSemi, *)].commutativeFlatMap[String, Int, String]
  )
  checkAll(
    "CommutativeFlatMap[(CSemi, CSemi, CSemi, CSemi, CSemi, *)] serializable",
    SerializableTests.serializable(CommutativeFlatMap[(CSemi, CSemi, CSemi, CSemi, CSemi, *)])
  )

  checkAll("CommutativeMonad[Tuple1]", CommutativeMonadTests[Tuple1].commutativeMonad[Int, Int, Int])
  checkAll("CommutativeMonad[Tuple1] serializable", SerializableTests.serializable(CommutativeMonad[Tuple1]))
  checkAll("CommutativeMonad[(Int, *)]", CommutativeMonadTests[(Int, *)].commutativeMonad[Int, Int, Int])
  checkAll("CommutativeMonad[(Int, *)] serializable", SerializableTests.serializable(CommutativeMonad[(Int, *)]))
  checkAll("CommutativeMonad[(CMono, CMono, Int, CMono, CMono, *)]",
           CommutativeMonadTests[(CMono, CMono, Int, CMono, CMono, *)].commutativeMonad[Int, Int, Int]
  )
  checkAll(
    "CommutativeMonad[(CMono, CMono, Int, CMono, CMono, *)] serializable",
    SerializableTests.serializable(CommutativeMonad[(CMono, CMono, Int, CMono, CMono, *)])
  )

  checkAll("Reducible[Tuple1]", ReducibleTests[Tuple1].reducible[Option, Int, Int])
  checkAll("Reducible[Tuple1]", SerializableTests.serializable(Reducible[Tuple1]))
  checkAll("Reducible[(String, *)]", ReducibleTests[(String, *)].reducible[Option, Int, Int])
  checkAll("Reducible[(String, *)]", SerializableTests.serializable(Reducible[(String, *)]))
  checkAll(
    "Reducible[(Boolean, Boolean, String, List[Int], NonEmptyList[Int], *)]",
    ReducibleTests[(Boolean, Boolean, String, List[Int], NonEmptyList[Int], *)].reducible[Option, Int, Int]
  )
  checkAll(
    "Reducible[(Boolean, Boolean, String, List[Int], NonEmptyList[Int], *)]",
    SerializableTests.serializable(Reducible[(Boolean, Boolean, String, List[Int], NonEmptyList[Int], *)])
  )

  test("Semigroupal composition") {
    val cart = ContravariantSemigroupal[Eq].composeFunctor[(Int, *)]
    val eq = cart.product(Eq[(Int, String)], Eq[(Int, Int)])
    forAll { (a: (Int, (String, Int)), b: (Int, (String, Int))) =>
      assert((a == b) === (eq.eqv(a, b)))
    }
  }

  test("eqv") {
    val eq = Eq[(Int, Long)]
    forAll { (t: (Int, Long)) =>
      assert(eq.eqv(t, t) === true)
    }
    forAll { (t: (Int, Long)) =>
      assert(eq.eqv(t, t._1 -> (t._2 + 1)) === false)
    }
  }

  test("order") {
    forAll { (t: (Int, Int)) =>
      val u = t.swap
      assert(Order[(Int, Int)].compare(t, u) === (scala.math.Ordering[(Int, Int)].compare(t, u)))
    }
  }

  test("Tuple instances inference") {
    import cats.InvariantMonoidal
    import cats.InvariantSemigroupal
    import cats.Functor
    import cats.Apply
    import cats.Applicative
    import cats.Foldable
    import cats.CommutativeApplicative

    Invariant[(String, Int, Boolean, Double, *)]
    InvariantSemigroupal[(Semi, Semi, Semi, Semi, *)]
    InvariantMonoidal[(Mono, Mono, Mono, Mono, *)]
    Foldable[(String, Int, Boolean, Double, *)]
    Traverse[(String, Int, Boolean, Double, List[Int], *)]
    Functor[(String, Int, Boolean, Double, List[Int], *)]
    Apply[(Semi, Semi, Semi, Semi, Semi, *)]
    FlatMap[(Semi, Semi, Semi, Semi, Semi, *)]
    Applicative[(Mono, Mono, Mono, Mono, *)]
    CommutativeApplicative[(CMono, CMono, CMono, CMono, CMono, *)]
  }

  test("Show syntax arity") {
    assert(Tuple1(1).show === "(1)")
    assert((1, 2).show === "(1,2)")
    assert((1, 2, 3).show === "(1,2,3)")
    assert((1, 2, 3, 4).show === "(1,2,3,4)")
    assert((1, 2, 3, 4, 5).show === "(1,2,3,4,5)")
    assert((1, 2, 3, 4, 5, 6).show === "(1,2,3,4,5,6)")
    assert((1, 2, 3, 4, 5, 6, 7).show === "(1,2,3,4,5,6,7)")
    assert((1, 2, 3, 4, 5, 6, 7, 8).show === "(1,2,3,4,5,6,7,8)")

    forAll { (fs: (String, String)) =>
      assert(fs.show === fs.toString)
    }
    forAll { (fs: (String, String, String, String, String)) =>
      assert(fs.show === fs.toString)
    }

    // Provide some "non-standard" Show instances to make sure the tuples actually use the Show instances for the
    // relevant types instead of blindly calling toString
    case class Foo(x: Int)
    object Foo {
      implicit val fooShow: Show[Foo] = (f: Foo) => s"foo.x = ${f.x}"
    }

    case class Bar(y: Int)
    object Bar {
      implicit val barShow: Show[Bar] = (f: Bar) => s"bar.y = ${f.y}"
    }

    case class Baz(y: Int)
    object Baz {
      implicit val bazShow: Show[Baz] = (f: Baz) => s"baz.y = ${f.y}"
    }

    val foo1 = Foo(1)
    val bar1 = Bar(2)
    val baz1 = Baz(3)
    val foo2 = Foo(4)
    val bar2 = Bar(5)
    val baz2 = Baz(6)
    val foo3 = Foo(7)
    val bar3 = Bar(8)
    val baz3 = Baz(9)

    assert(Tuple1(foo1).show === s"(${Show[Foo].show(foo1)})")
    assert((foo1, bar1).show === s"(${Show[Foo].show(foo1)},${Show[Bar].show(bar1)})")
    assert((foo1, bar1, baz1).show === s"(${Show[Foo].show(foo1)},${Show[Bar].show(bar1)},${Show[Baz].show(baz1)})")
    assert((foo1, bar1, baz1, foo2, bar2, baz2).show === s"(${Show[Foo].show(foo1)},${Show[Bar].show(bar1)},${Show[Baz]
        .show(baz1)},${Show[Foo].show(foo2)},${Show[Bar].show(bar2)},${Show[Baz].show(baz2)})")
    assert((foo1, bar1, baz1, foo2, bar2, baz2, foo3, bar3, baz3).show === s"(${Show[Foo].show(foo1)},${Show[Bar]
        .show(bar1)},${Show[Baz].show(baz1)},${Show[Foo].show(foo2)},${Show[Bar].show(bar2)},${Show[Baz]
        .show(baz2)},${Show[Foo].show(foo3)},${Show[Bar].show(bar3)},${Show[Baz].show(baz3)})")
  }
}
