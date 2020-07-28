package cats.tests

import cats.{Contravariant, ContravariantMonoidal, Monad, Semigroupal}
import cats.arrow.{ArrowChoice, Choice, CommutativeArrow}
import cats.data.AndThen
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.platform.Platform
import org.scalacheck.Prop._

class AndThenSuite extends CatsSuite {
  checkAll("AndThen[MiniInt, Int]", SemigroupalTests[AndThen[MiniInt, *]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[AndThen[Int, *]]", SerializableTests.serializable(Semigroupal[AndThen[Int, *]]))

  {
    implicit val iso: SemigroupalTests.Isomorphisms[AndThen[*, Int]] =
      SemigroupalTests.Isomorphisms.invariant[AndThen[*, Int]]
    checkAll("AndThen[*, Int]",
             ContravariantMonoidalTests[AndThen[*, Int]].contravariantMonoidal[MiniInt, Boolean, Boolean]
    )
    checkAll("ContravariantMonoidal[AndThen[*, Int]]",
             SerializableTests.serializable(ContravariantMonoidal[AndThen[*, Int]])
    )
  }

  checkAll("AndThen[MiniInt, Int]", MonadTests[AndThen[MiniInt, *]].monad[Int, Int, Int])
  checkAll("Monad[AndThen[Int, *]]", SerializableTests.serializable(Monad[AndThen[Int, *]]))

  checkAll("AndThen",
           CommutativeArrowTests[AndThen].commutativeArrow[MiniInt, Boolean, Boolean, Boolean, Boolean, Boolean]
  )
  checkAll("Arrow[AndThen]", SerializableTests.serializable(CommutativeArrow[AndThen]))

  checkAll("AndThen", ChoiceTests[AndThen].choice[MiniInt, Boolean, Int, Int])
  checkAll("Choice[AndThen]", SerializableTests.serializable(Choice[AndThen]))

  checkAll("AndThen", ArrowChoiceTests[AndThen].arrowChoice[MiniInt, Boolean, Boolean, Boolean, Boolean, Boolean])
  checkAll("ArrowChoice[AndThen]", SerializableTests.serializable(ArrowChoice[AndThen]))

  checkAll("AndThen[*, Int]", ContravariantTests[AndThen[*, Int]].contravariant[MiniInt, Int, Boolean])
  checkAll("Contravariant[AndThen[*, Int]]", SerializableTests.serializable(Contravariant[AndThen[*, Int]]))

  property("compose a chain of functions with andThen") {
    forAll { (i: Int, fs: List[Int => Int]) =>
      val result = fs.map(AndThen(_)).reduceOption(_.andThen(_)).map(_(i))
      val expect = fs.reduceOption(_.andThen(_)).map(_(i))

      result == expect
    }
  }

  property("compose a chain of functions with compose") {
    forAll { (i: Int, fs: List[Int => Int]) =>
      val result = fs.map(AndThen(_)).reduceOption(_.compose(_)).map(_(i))
      val expect = fs.reduceOption(_.compose(_)).map(_(i))

      result == expect
    }
  }

  test("andThen is stack safe") {
    val count = if (Platform.isJvm) 500000 else 1000
    val fs = (0 until count).map(_ => (i: Int) => i + 1)
    val result = fs.foldLeft(AndThen((x: Int) => x))(_.andThen(_))(42)

    assertEquals(result, (count + 42))
  }

  test("compose is stack safe") {
    val count = if (Platform.isJvm) 500000 else 1000
    val fs = (0 until count).map(_ => (i: Int) => i + 1)
    val result = fs.foldLeft(AndThen((x: Int) => x))(_.compose(_))(42)

    assertEquals(result, (count + 42))
  }

  test("Function1 andThen is stack safe") {
    val count = if (Platform.isJvm) 50000 else 1000
    val start: (Int => Int) = AndThen((x: Int) => x)
    val fs = (0 until count).foldLeft(start) { (acc, _) =>
      acc.andThen(_ + 1)
    }
    assertEquals(fs(0), count)
  }

  test("toString") {
    assert(AndThen((x: Int) => x).toString.startsWith("AndThen$"))
  }
}
