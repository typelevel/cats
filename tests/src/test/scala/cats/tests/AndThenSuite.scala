package cats
package tests

import cats.data._
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline._
import cats.arrow._
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import cats.platform.Platform

class AndThenSuite extends CatsSuite {
  checkAll("AndThen[MiniInt, Int]", SemigroupalTests[AndThen[MiniInt, ?]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[AndThen[Int, ?]]", SerializableTests.serializable(Semigroupal[AndThen[Int, ?]]))

  {
    implicit val iso = SemigroupalTests.Isomorphisms.invariant[AndThen[?, Int]]
    checkAll("AndThen[?, Int]",
             ContravariantMonoidalTests[AndThen[?, Int]].contravariantMonoidal[MiniInt, Boolean, Boolean])
    checkAll("ContravariantMonoidal[AndThen[?, Int]]",
             SerializableTests.serializable(ContravariantMonoidal[AndThen[?, Int]]))
  }

  checkAll("AndThen[MiniInt, Int]", MonadTests[AndThen[MiniInt, ?]].monad[Int, Int, Int])
  checkAll("Monad[AndThen[Int, ?]]", SerializableTests.serializable(Monad[AndThen[Int, ?]]))

  checkAll("AndThen",
           CommutativeArrowTests[AndThen].commutativeArrow[MiniInt, Boolean, Boolean, Boolean, Boolean, Boolean])
  checkAll("Arrow[AndThen]", SerializableTests.serializable(CommutativeArrow[AndThen]))

  checkAll("AndThen", ChoiceTests[AndThen].choice[MiniInt, Boolean, Int, Int])
  checkAll("Choice[AndThen]", SerializableTests.serializable(Choice[AndThen]))

  checkAll("AndThen", ArrowChoiceTests[AndThen].arrowChoice[MiniInt, Boolean, Boolean, Boolean, Boolean, Boolean])
  checkAll("ArrowChoice[AndThen]", SerializableTests.serializable(ArrowChoice[AndThen]))

  checkAll("AndThen[?, Int]", ContravariantTests[AndThen[?, Int]].contravariant[MiniInt, Int, Boolean])
  checkAll("Contravariant[AndThen[?, Int]]", SerializableTests.serializable(Contravariant[AndThen[?, Int]]))

  test("compose a chain of functions with andThen") {
    check { (i: Int, fs: List[Int => Int]) =>
      val result = fs.map(AndThen(_)).reduceOption(_.andThen(_)).map(_(i))
      val expect = fs.reduceOption(_.andThen(_)).map(_(i))

      result == expect
    }
  }

  test("compose a chain of functions with compose") {
    check { (i: Int, fs: List[Int => Int]) =>
      val result = fs.map(AndThen(_)).reduceOption(_.compose(_)).map(_(i))
      val expect = fs.reduceOption(_.compose(_)).map(_(i))

      result == expect
    }
  }

  test("andThen is stack safe") {
    val count = if (Platform.isJvm) 500000 else 1000
    val fs = (0 until count).map(_ => { i: Int =>
      i + 1
    })
    val result = fs.foldLeft(AndThen((x: Int) => x))(_.andThen(_))(42)

    result shouldEqual (count + 42)
  }

  test("compose is stack safe") {
    val count = if (Platform.isJvm) 500000 else 1000
    val fs = (0 until count).map(_ => { i: Int =>
      i + 1
    })
    val result = fs.foldLeft(AndThen((x: Int) => x))(_.compose(_))(42)

    result shouldEqual (count + 42)
  }

  test("Function1 andThen is stack safe") {
    val count = if (Platform.isJvm) 50000 else 1000
    val start: (Int => Int) = AndThen((x: Int) => x)
    val fs = (0 until count).foldLeft(start) { (acc, _) =>
      acc.andThen(_ + 1)
    }
    fs(0) shouldEqual count
  }

  test("toString") {
    AndThen((x: Int) => x).toString should startWith("AndThen$")
  }
}
